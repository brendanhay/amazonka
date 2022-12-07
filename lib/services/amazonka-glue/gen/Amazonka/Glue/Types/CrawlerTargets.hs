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
-- Module      : Amazonka.Glue.Types.CrawlerTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerTargets where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CatalogTarget
import Amazonka.Glue.Types.DeltaTarget
import Amazonka.Glue.Types.DynamoDBTarget
import Amazonka.Glue.Types.JdbcTarget
import Amazonka.Glue.Types.MongoDBTarget
import Amazonka.Glue.Types.S3Target
import qualified Amazonka.Prelude as Prelude

-- | Specifies data stores to crawl.
--
-- /See:/ 'newCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { -- | Specifies Amazon DocumentDB or MongoDB targets.
    mongoDBTargets :: Prelude.Maybe [MongoDBTarget],
    -- | Specifies Amazon DynamoDB targets.
    dynamoDBTargets :: Prelude.Maybe [DynamoDBTarget],
    -- | Specifies Delta data store targets.
    deltaTargets :: Prelude.Maybe [DeltaTarget],
    -- | Specifies JDBC targets.
    jdbcTargets :: Prelude.Maybe [JdbcTarget],
    -- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
    s3Targets :: Prelude.Maybe [S3Target],
    -- | Specifies Glue Data Catalog targets.
    catalogTargets :: Prelude.Maybe [CatalogTarget]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrawlerTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mongoDBTargets', 'crawlerTargets_mongoDBTargets' - Specifies Amazon DocumentDB or MongoDB targets.
--
-- 'dynamoDBTargets', 'crawlerTargets_dynamoDBTargets' - Specifies Amazon DynamoDB targets.
--
-- 'deltaTargets', 'crawlerTargets_deltaTargets' - Specifies Delta data store targets.
--
-- 'jdbcTargets', 'crawlerTargets_jdbcTargets' - Specifies JDBC targets.
--
-- 's3Targets', 'crawlerTargets_s3Targets' - Specifies Amazon Simple Storage Service (Amazon S3) targets.
--
-- 'catalogTargets', 'crawlerTargets_catalogTargets' - Specifies Glue Data Catalog targets.
newCrawlerTargets ::
  CrawlerTargets
newCrawlerTargets =
  CrawlerTargets'
    { mongoDBTargets = Prelude.Nothing,
      dynamoDBTargets = Prelude.Nothing,
      deltaTargets = Prelude.Nothing,
      jdbcTargets = Prelude.Nothing,
      s3Targets = Prelude.Nothing,
      catalogTargets = Prelude.Nothing
    }

-- | Specifies Amazon DocumentDB or MongoDB targets.
crawlerTargets_mongoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [MongoDBTarget])
crawlerTargets_mongoDBTargets = Lens.lens (\CrawlerTargets' {mongoDBTargets} -> mongoDBTargets) (\s@CrawlerTargets' {} a -> s {mongoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Amazon DynamoDB targets.
crawlerTargets_dynamoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [DynamoDBTarget])
crawlerTargets_dynamoDBTargets = Lens.lens (\CrawlerTargets' {dynamoDBTargets} -> dynamoDBTargets) (\s@CrawlerTargets' {} a -> s {dynamoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Delta data store targets.
crawlerTargets_deltaTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [DeltaTarget])
crawlerTargets_deltaTargets = Lens.lens (\CrawlerTargets' {deltaTargets} -> deltaTargets) (\s@CrawlerTargets' {} a -> s {deltaTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies JDBC targets.
crawlerTargets_jdbcTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [JdbcTarget])
crawlerTargets_jdbcTargets = Lens.lens (\CrawlerTargets' {jdbcTargets} -> jdbcTargets) (\s@CrawlerTargets' {} a -> s {jdbcTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
crawlerTargets_s3Targets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [S3Target])
crawlerTargets_s3Targets = Lens.lens (\CrawlerTargets' {s3Targets} -> s3Targets) (\s@CrawlerTargets' {} a -> s {s3Targets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Glue Data Catalog targets.
crawlerTargets_catalogTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [CatalogTarget])
crawlerTargets_catalogTargets = Lens.lens (\CrawlerTargets' {catalogTargets} -> catalogTargets) (\s@CrawlerTargets' {} a -> s {catalogTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CrawlerTargets where
  parseJSON =
    Data.withObject
      "CrawlerTargets"
      ( \x ->
          CrawlerTargets'
            Prelude.<$> (x Data..:? "MongoDBTargets" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "DynamoDBTargets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeltaTargets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "JdbcTargets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "S3Targets" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "CatalogTargets"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CrawlerTargets where
  hashWithSalt _salt CrawlerTargets' {..} =
    _salt `Prelude.hashWithSalt` mongoDBTargets
      `Prelude.hashWithSalt` dynamoDBTargets
      `Prelude.hashWithSalt` deltaTargets
      `Prelude.hashWithSalt` jdbcTargets
      `Prelude.hashWithSalt` s3Targets
      `Prelude.hashWithSalt` catalogTargets

instance Prelude.NFData CrawlerTargets where
  rnf CrawlerTargets' {..} =
    Prelude.rnf mongoDBTargets
      `Prelude.seq` Prelude.rnf dynamoDBTargets
      `Prelude.seq` Prelude.rnf deltaTargets
      `Prelude.seq` Prelude.rnf jdbcTargets
      `Prelude.seq` Prelude.rnf s3Targets
      `Prelude.seq` Prelude.rnf catalogTargets

instance Data.ToJSON CrawlerTargets where
  toJSON CrawlerTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MongoDBTargets" Data..=)
              Prelude.<$> mongoDBTargets,
            ("DynamoDBTargets" Data..=)
              Prelude.<$> dynamoDBTargets,
            ("DeltaTargets" Data..=) Prelude.<$> deltaTargets,
            ("JdbcTargets" Data..=) Prelude.<$> jdbcTargets,
            ("S3Targets" Data..=) Prelude.<$> s3Targets,
            ("CatalogTargets" Data..=)
              Prelude.<$> catalogTargets
          ]
      )
