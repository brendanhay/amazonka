{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.MongoDBTarget
import Network.AWS.Glue.Types.S3Target
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies data stores to crawl.
--
-- /See:/ 'newCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { -- | Specifies AWS Glue Data Catalog targets.
    catalogTargets :: Prelude.Maybe [CatalogTarget],
    -- | Specifies Amazon DocumentDB or MongoDB targets.
    mongoDBTargets :: Prelude.Maybe [MongoDBTarget],
    -- | Specifies Amazon DynamoDB targets.
    dynamoDBTargets :: Prelude.Maybe [DynamoDBTarget],
    -- | Specifies JDBC targets.
    jdbcTargets :: Prelude.Maybe [JdbcTarget],
    -- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
    s3Targets :: Prelude.Maybe [S3Target]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { catalogTargets = Prelude.Nothing,
      mongoDBTargets = Prelude.Nothing,
      dynamoDBTargets = Prelude.Nothing,
      jdbcTargets = Prelude.Nothing,
      s3Targets = Prelude.Nothing
    }

-- | Specifies AWS Glue Data Catalog targets.
crawlerTargets_catalogTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [CatalogTarget])
crawlerTargets_catalogTargets = Lens.lens (\CrawlerTargets' {catalogTargets} -> catalogTargets) (\s@CrawlerTargets' {} a -> s {catalogTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies Amazon DocumentDB or MongoDB targets.
crawlerTargets_mongoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [MongoDBTarget])
crawlerTargets_mongoDBTargets = Lens.lens (\CrawlerTargets' {mongoDBTargets} -> mongoDBTargets) (\s@CrawlerTargets' {} a -> s {mongoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies Amazon DynamoDB targets.
crawlerTargets_dynamoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [DynamoDBTarget])
crawlerTargets_dynamoDBTargets = Lens.lens (\CrawlerTargets' {dynamoDBTargets} -> dynamoDBTargets) (\s@CrawlerTargets' {} a -> s {dynamoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies JDBC targets.
crawlerTargets_jdbcTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [JdbcTarget])
crawlerTargets_jdbcTargets = Lens.lens (\CrawlerTargets' {jdbcTargets} -> jdbcTargets) (\s@CrawlerTargets' {} a -> s {jdbcTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
crawlerTargets_s3Targets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [S3Target])
crawlerTargets_s3Targets = Lens.lens (\CrawlerTargets' {s3Targets} -> s3Targets) (\s@CrawlerTargets' {} a -> s {s3Targets = a} :: CrawlerTargets) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CrawlerTargets where
  parseJSON =
    Prelude.withObject
      "CrawlerTargets"
      ( \x ->
          CrawlerTargets'
            Prelude.<$> ( x Prelude..:? "CatalogTargets"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "MongoDBTargets"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "DynamoDBTargets"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "JdbcTargets"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "S3Targets"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CrawlerTargets

instance Prelude.NFData CrawlerTargets

instance Prelude.ToJSON CrawlerTargets where
  toJSON CrawlerTargets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogTargets" Prelude..=)
              Prelude.<$> catalogTargets,
            ("MongoDBTargets" Prelude..=)
              Prelude.<$> mongoDBTargets,
            ("DynamoDBTargets" Prelude..=)
              Prelude.<$> dynamoDBTargets,
            ("JdbcTargets" Prelude..=) Prelude.<$> jdbcTargets,
            ("S3Targets" Prelude..=) Prelude.<$> s3Targets
          ]
      )
