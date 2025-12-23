#!/bin/bash

rsync -avh --exclude='*.git' --exclude='.Rhistory' --exclude='.gitignore' --exclude='.Rproj.user' --exclude='Rproj.user/*' --exclude='*.Rproj' --exclude='.Rdata' --exclude='*publish.sh' ./ /datasets/work/sc-shiny/work/live_apps/BitbucketMigration/BitbucketMigrationOwnershipApp



