{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerTargets where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.MongoDBTarget
import Network.AWS.Glue.Types.S3Target
import qualified Network.AWS.Lens as Lens

-- | Specifies data stores to crawl.
--
-- /See:/ 'newCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { -- | Specifies AWS Glue Data Catalog targets.
    catalogTargets :: Core.Maybe [CatalogTarget],
    -- | Specifies Amazon DocumentDB or MongoDB targets.
    mongoDBTargets :: Core.Maybe [MongoDBTarget],
    -- | Specifies Amazon DynamoDB targets.
    dynamoDBTargets :: Core.Maybe [DynamoDBTarget],
    -- | Specifies JDBC targets.
    jdbcTargets :: Core.Maybe [JdbcTarget],
    -- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
    s3Targets :: Core.Maybe [S3Target]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CrawlerTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogTargets', 'crawlerTargets_catalogTargets' - Specifies AWS Glue Data Catalog targets.
--
-- 'mongoDBTargets', 'crawlerTargets_mongoDBTargets' - Specifies Amazon DocumentDB or MongoDB targets.
--
-- 'dynamoDBTargets', 'crawlerTargets_dynamoDBTargets' - Specifies Amazon DynamoDB targets.
--
-- 'jdbcTargets', 'crawlerTargets_jdbcTargets' - Specifies JDBC targets.
--
-- 's3Targets', 'crawlerTargets_s3Targets' - Specifies Amazon Simple Storage Service (Amazon S3) targets.
newCrawlerTargets ::
  CrawlerTargets
newCrawlerTargets =
  CrawlerTargets'
    { catalogTargets = Core.Nothing,
      mongoDBTargets = Core.Nothing,
      dynamoDBTargets = Core.Nothing,
      jdbcTargets = Core.Nothing,
      s3Targets = Core.Nothing
    }

-- | Specifies AWS Glue Data Catalog targets.
crawlerTargets_catalogTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [CatalogTarget])
crawlerTargets_catalogTargets = Lens.lens (\CrawlerTargets' {catalogTargets} -> catalogTargets) (\s@CrawlerTargets' {} a -> s {catalogTargets = a} :: CrawlerTargets) Core.. Lens.mapping Lens._Coerce

-- | Specifies Amazon DocumentDB or MongoDB targets.
crawlerTargets_mongoDBTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [MongoDBTarget])
crawlerTargets_mongoDBTargets = Lens.lens (\CrawlerTargets' {mongoDBTargets} -> mongoDBTargets) (\s@CrawlerTargets' {} a -> s {mongoDBTargets = a} :: CrawlerTargets) Core.. Lens.mapping Lens._Coerce

-- | Specifies Amazon DynamoDB targets.
crawlerTargets_dynamoDBTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [DynamoDBTarget])
crawlerTargets_dynamoDBTargets = Lens.lens (\CrawlerTargets' {dynamoDBTargets} -> dynamoDBTargets) (\s@CrawlerTargets' {} a -> s {dynamoDBTargets = a} :: CrawlerTargets) Core.. Lens.mapping Lens._Coerce

-- | Specifies JDBC targets.
crawlerTargets_jdbcTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [JdbcTarget])
crawlerTargets_jdbcTargets = Lens.lens (\CrawlerTargets' {jdbcTargets} -> jdbcTargets) (\s@CrawlerTargets' {} a -> s {jdbcTargets = a} :: CrawlerTargets) Core.. Lens.mapping Lens._Coerce

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
crawlerTargets_s3Targets :: Lens.Lens' CrawlerTargets (Core.Maybe [S3Target])
crawlerTargets_s3Targets = Lens.lens (\CrawlerTargets' {s3Targets} -> s3Targets) (\s@CrawlerTargets' {} a -> s {s3Targets = a} :: CrawlerTargets) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CrawlerTargets where
  parseJSON =
    Core.withObject
      "CrawlerTargets"
      ( \x ->
          CrawlerTargets'
            Core.<$> (x Core..:? "CatalogTargets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MongoDBTargets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DynamoDBTargets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "JdbcTargets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "S3Targets" Core..!= Core.mempty)
      )

instance Core.Hashable CrawlerTargets

instance Core.NFData CrawlerTargets

instance Core.ToJSON CrawlerTargets where
  toJSON CrawlerTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogTargets" Core..=) Core.<$> catalogTargets,
            ("MongoDBTargets" Core..=) Core.<$> mongoDBTargets,
            ("DynamoDBTargets" Core..=) Core.<$> dynamoDBTargets,
            ("JdbcTargets" Core..=) Core.<$> jdbcTargets,
            ("S3Targets" Core..=) Core.<$> s3Targets
          ]
      )
