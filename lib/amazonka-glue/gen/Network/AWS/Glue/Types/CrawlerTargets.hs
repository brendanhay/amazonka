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
    ctDynamoDBTargets,
    ctS3Targets,
    ctMongoDBTargets,
    ctCatalogTargets,
    ctJdbcTargets,
  )
where

import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.MongoDBTarget
import Network.AWS.Glue.Types.S3Target
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies data stores to crawl.
--
-- /See:/ 'mkCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { dynamoDBTargets ::
      Lude.Maybe [DynamoDBTarget],
    s3Targets :: Lude.Maybe [S3Target],
    mongoDBTargets :: Lude.Maybe [MongoDBTarget],
    catalogTargets :: Lude.Maybe [CatalogTarget],
    jdbcTargets :: Lude.Maybe [JdbcTarget]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CrawlerTargets' with the minimum fields required to make a request.
--
-- * 'catalogTargets' - Specifies AWS Glue Data Catalog targets.
-- * 'dynamoDBTargets' - Specifies Amazon DynamoDB targets.
-- * 'jdbcTargets' - Specifies JDBC targets.
-- * 'mongoDBTargets' - Specifies Amazon DocumentDB or MongoDB targets.
-- * 's3Targets' - Specifies Amazon Simple Storage Service (Amazon S3) targets.
mkCrawlerTargets ::
  CrawlerTargets
mkCrawlerTargets =
  CrawlerTargets'
    { dynamoDBTargets = Lude.Nothing,
      s3Targets = Lude.Nothing,
      mongoDBTargets = Lude.Nothing,
      catalogTargets = Lude.Nothing,
      jdbcTargets = Lude.Nothing
    }

-- | Specifies Amazon DynamoDB targets.
--
-- /Note:/ Consider using 'dynamoDBTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDynamoDBTargets :: Lens.Lens' CrawlerTargets (Lude.Maybe [DynamoDBTarget])
ctDynamoDBTargets = Lens.lens (dynamoDBTargets :: CrawlerTargets -> Lude.Maybe [DynamoDBTarget]) (\s a -> s {dynamoDBTargets = a} :: CrawlerTargets)
{-# DEPRECATED ctDynamoDBTargets "Use generic-lens or generic-optics with 'dynamoDBTargets' instead." #-}

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
--
-- /Note:/ Consider using 's3Targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctS3Targets :: Lens.Lens' CrawlerTargets (Lude.Maybe [S3Target])
ctS3Targets = Lens.lens (s3Targets :: CrawlerTargets -> Lude.Maybe [S3Target]) (\s a -> s {s3Targets = a} :: CrawlerTargets)
{-# DEPRECATED ctS3Targets "Use generic-lens or generic-optics with 's3Targets' instead." #-}

-- | Specifies Amazon DocumentDB or MongoDB targets.
--
-- /Note:/ Consider using 'mongoDBTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctMongoDBTargets :: Lens.Lens' CrawlerTargets (Lude.Maybe [MongoDBTarget])
ctMongoDBTargets = Lens.lens (mongoDBTargets :: CrawlerTargets -> Lude.Maybe [MongoDBTarget]) (\s a -> s {mongoDBTargets = a} :: CrawlerTargets)
{-# DEPRECATED ctMongoDBTargets "Use generic-lens or generic-optics with 'mongoDBTargets' instead." #-}

-- | Specifies AWS Glue Data Catalog targets.
--
-- /Note:/ Consider using 'catalogTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctCatalogTargets :: Lens.Lens' CrawlerTargets (Lude.Maybe [CatalogTarget])
ctCatalogTargets = Lens.lens (catalogTargets :: CrawlerTargets -> Lude.Maybe [CatalogTarget]) (\s a -> s {catalogTargets = a} :: CrawlerTargets)
{-# DEPRECATED ctCatalogTargets "Use generic-lens or generic-optics with 'catalogTargets' instead." #-}

-- | Specifies JDBC targets.
--
-- /Note:/ Consider using 'jdbcTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctJdbcTargets :: Lens.Lens' CrawlerTargets (Lude.Maybe [JdbcTarget])
ctJdbcTargets = Lens.lens (jdbcTargets :: CrawlerTargets -> Lude.Maybe [JdbcTarget]) (\s a -> s {jdbcTargets = a} :: CrawlerTargets)
{-# DEPRECATED ctJdbcTargets "Use generic-lens or generic-optics with 'jdbcTargets' instead." #-}

instance Lude.FromJSON CrawlerTargets where
  parseJSON =
    Lude.withObject
      "CrawlerTargets"
      ( \x ->
          CrawlerTargets'
            Lude.<$> (x Lude..:? "DynamoDBTargets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "S3Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MongoDBTargets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CatalogTargets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "JdbcTargets" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CrawlerTargets where
  toJSON CrawlerTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DynamoDBTargets" Lude..=) Lude.<$> dynamoDBTargets,
            ("S3Targets" Lude..=) Lude.<$> s3Targets,
            ("MongoDBTargets" Lude..=) Lude.<$> mongoDBTargets,
            ("CatalogTargets" Lude..=) Lude.<$> catalogTargets,
            ("JdbcTargets" Lude..=) Lude.<$> jdbcTargets
          ]
      )
