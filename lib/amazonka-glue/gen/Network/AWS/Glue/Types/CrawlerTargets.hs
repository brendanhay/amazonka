{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerTargets
  ( CrawlerTargets (..),

    -- * Smart constructor
    mkCrawlerTargets,

    -- * Lenses
    ctCatalogTargets,
    ctDynamoDBTargets,
    ctJdbcTargets,
    ctMongoDBTargets,
    ctS3Targets,
  )
where

import qualified Network.AWS.Glue.Types.CatalogTarget as Types
import qualified Network.AWS.Glue.Types.DynamoDBTarget as Types
import qualified Network.AWS.Glue.Types.JdbcTarget as Types
import qualified Network.AWS.Glue.Types.MongoDBTarget as Types
import qualified Network.AWS.Glue.Types.S3Target as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies data stores to crawl.
--
-- /See:/ 'mkCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { -- | Specifies AWS Glue Data Catalog targets.
    catalogTargets :: Core.Maybe [Types.CatalogTarget],
    -- | Specifies Amazon DynamoDB targets.
    dynamoDBTargets :: Core.Maybe [Types.DynamoDBTarget],
    -- | Specifies JDBC targets.
    jdbcTargets :: Core.Maybe [Types.JdbcTarget],
    -- | Specifies Amazon DocumentDB or MongoDB targets.
    mongoDBTargets :: Core.Maybe [Types.MongoDBTarget],
    -- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
    s3Targets :: Core.Maybe [Types.S3Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CrawlerTargets' value with any optional fields omitted.
mkCrawlerTargets ::
  CrawlerTargets
mkCrawlerTargets =
  CrawlerTargets'
    { catalogTargets = Core.Nothing,
      dynamoDBTargets = Core.Nothing,
      jdbcTargets = Core.Nothing,
      mongoDBTargets = Core.Nothing,
      s3Targets = Core.Nothing
    }

-- | Specifies AWS Glue Data Catalog targets.
--
-- /Note:/ Consider using 'catalogTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCatalogTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [Types.CatalogTarget])
ctCatalogTargets = Lens.field @"catalogTargets"
{-# DEPRECATED ctCatalogTargets "Use generic-lens or generic-optics with 'catalogTargets' instead." #-}

-- | Specifies Amazon DynamoDB targets.
--
-- /Note:/ Consider using 'dynamoDBTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDynamoDBTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [Types.DynamoDBTarget])
ctDynamoDBTargets = Lens.field @"dynamoDBTargets"
{-# DEPRECATED ctDynamoDBTargets "Use generic-lens or generic-optics with 'dynamoDBTargets' instead." #-}

-- | Specifies JDBC targets.
--
-- /Note:/ Consider using 'jdbcTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctJdbcTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [Types.JdbcTarget])
ctJdbcTargets = Lens.field @"jdbcTargets"
{-# DEPRECATED ctJdbcTargets "Use generic-lens or generic-optics with 'jdbcTargets' instead." #-}

-- | Specifies Amazon DocumentDB or MongoDB targets.
--
-- /Note:/ Consider using 'mongoDBTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctMongoDBTargets :: Lens.Lens' CrawlerTargets (Core.Maybe [Types.MongoDBTarget])
ctMongoDBTargets = Lens.field @"mongoDBTargets"
{-# DEPRECATED ctMongoDBTargets "Use generic-lens or generic-optics with 'mongoDBTargets' instead." #-}

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
--
-- /Note:/ Consider using 's3Targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3Targets :: Lens.Lens' CrawlerTargets (Core.Maybe [Types.S3Target])
ctS3Targets = Lens.field @"s3Targets"
{-# DEPRECATED ctS3Targets "Use generic-lens or generic-optics with 's3Targets' instead." #-}

instance Core.FromJSON CrawlerTargets where
  toJSON CrawlerTargets {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogTargets" Core..=) Core.<$> catalogTargets,
            ("DynamoDBTargets" Core..=) Core.<$> dynamoDBTargets,
            ("JdbcTargets" Core..=) Core.<$> jdbcTargets,
            ("MongoDBTargets" Core..=) Core.<$> mongoDBTargets,
            ("S3Targets" Core..=) Core.<$> s3Targets
          ]
      )

instance Core.FromJSON CrawlerTargets where
  parseJSON =
    Core.withObject "CrawlerTargets" Core.$
      \x ->
        CrawlerTargets'
          Core.<$> (x Core..:? "CatalogTargets")
          Core.<*> (x Core..:? "DynamoDBTargets")
          Core.<*> (x Core..:? "JdbcTargets")
          Core.<*> (x Core..:? "MongoDBTargets")
          Core.<*> (x Core..:? "S3Targets")
