#!/bin/bash

rsync -avh --exclude='*.git' --exclude='*.Rproj' --exclude=".Rdata" --exclude="*publish.sh" ./ /datasets/work/sc-shiny/work/live_apps/BitbucketMigration/BitbucketMigrationOwnershipApp



