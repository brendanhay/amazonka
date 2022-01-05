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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerTargets where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.CatalogTarget
import Amazonka.Glue.Types.DynamoDBTarget
import Amazonka.Glue.Types.JdbcTarget
import Amazonka.Glue.Types.MongoDBTarget
import Amazonka.Glue.Types.S3Target
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies data stores to crawl.
--
-- /See:/ 'newCrawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { -- | Specifies Amazon DynamoDB targets.
    dynamoDBTargets :: Prelude.Maybe [DynamoDBTarget],
    -- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
    s3Targets :: Prelude.Maybe [S3Target],
    -- | Specifies Amazon DocumentDB or MongoDB targets.
    mongoDBTargets :: Prelude.Maybe [MongoDBTarget],
    -- | Specifies Glue Data Catalog targets.
    catalogTargets :: Prelude.Maybe [CatalogTarget],
    -- | Specifies JDBC targets.
    jdbcTargets :: Prelude.Maybe [JdbcTarget]
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
-- 'dynamoDBTargets', 'crawlerTargets_dynamoDBTargets' - Specifies Amazon DynamoDB targets.
--
-- 's3Targets', 'crawlerTargets_s3Targets' - Specifies Amazon Simple Storage Service (Amazon S3) targets.
--
-- 'mongoDBTargets', 'crawlerTargets_mongoDBTargets' - Specifies Amazon DocumentDB or MongoDB targets.
--
-- 'catalogTargets', 'crawlerTargets_catalogTargets' - Specifies Glue Data Catalog targets.
--
-- 'jdbcTargets', 'crawlerTargets_jdbcTargets' - Specifies JDBC targets.
newCrawlerTargets ::
  CrawlerTargets
newCrawlerTargets =
  CrawlerTargets'
    { dynamoDBTargets = Prelude.Nothing,
      s3Targets = Prelude.Nothing,
      mongoDBTargets = Prelude.Nothing,
      catalogTargets = Prelude.Nothing,
      jdbcTargets = Prelude.Nothing
    }

-- | Specifies Amazon DynamoDB targets.
crawlerTargets_dynamoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [DynamoDBTarget])
crawlerTargets_dynamoDBTargets = Lens.lens (\CrawlerTargets' {dynamoDBTargets} -> dynamoDBTargets) (\s@CrawlerTargets' {} a -> s {dynamoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Amazon Simple Storage Service (Amazon S3) targets.
crawlerTargets_s3Targets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [S3Target])
crawlerTargets_s3Targets = Lens.lens (\CrawlerTargets' {s3Targets} -> s3Targets) (\s@CrawlerTargets' {} a -> s {s3Targets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Amazon DocumentDB or MongoDB targets.
crawlerTargets_mongoDBTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [MongoDBTarget])
crawlerTargets_mongoDBTargets = Lens.lens (\CrawlerTargets' {mongoDBTargets} -> mongoDBTargets) (\s@CrawlerTargets' {} a -> s {mongoDBTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies Glue Data Catalog targets.
crawlerTargets_catalogTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [CatalogTarget])
crawlerTargets_catalogTargets = Lens.lens (\CrawlerTargets' {catalogTargets} -> catalogTargets) (\s@CrawlerTargets' {} a -> s {catalogTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

-- | Specifies JDBC targets.
crawlerTargets_jdbcTargets :: Lens.Lens' CrawlerTargets (Prelude.Maybe [JdbcTarget])
crawlerTargets_jdbcTargets = Lens.lens (\CrawlerTargets' {jdbcTargets} -> jdbcTargets) (\s@CrawlerTargets' {} a -> s {jdbcTargets = a} :: CrawlerTargets) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CrawlerTargets where
  parseJSON =
    Core.withObject
      "CrawlerTargets"
      ( \x ->
          CrawlerTargets'
            Prelude.<$> ( x Core..:? "DynamoDBTargets"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "S3Targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MongoDBTargets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CatalogTargets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "JdbcTargets" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CrawlerTargets where
  hashWithSalt _salt CrawlerTargets' {..} =
    _salt `Prelude.hashWithSalt` dynamoDBTargets
      `Prelude.hashWithSalt` s3Targets
      `Prelude.hashWithSalt` mongoDBTargets
      `Prelude.hashWithSalt` catalogTargets
      `Prelude.hashWithSalt` jdbcTargets

instance Prelude.NFData CrawlerTargets where
  rnf CrawlerTargets' {..} =
    Prelude.rnf dynamoDBTargets
      `Prelude.seq` Prelude.rnf s3Targets
      `Prelude.seq` Prelude.rnf mongoDBTargets
      `Prelude.seq` Prelude.rnf catalogTargets
      `Prelude.seq` Prelude.rnf jdbcTargets

instance Core.ToJSON CrawlerTargets where
  toJSON CrawlerTargets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DynamoDBTargets" Core..=)
              Prelude.<$> dynamoDBTargets,
            ("S3Targets" Core..=) Prelude.<$> s3Targets,
            ("MongoDBTargets" Core..=)
              Prelude.<$> mongoDBTargets,
            ("CatalogTargets" Core..=)
              Prelude.<$> catalogTargets,
            ("JdbcTargets" Core..=) Prelude.<$> jdbcTargets
          ]
      )
