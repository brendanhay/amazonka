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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CatalogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CatalogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration parameters for the default Amazon Glue database. You
-- use this database for SQL queries that you write in a Kinesis Data
-- Analytics Studio notebook.
--
-- /See:/ 'newCatalogConfiguration' smart constructor.
data CatalogConfiguration = CatalogConfiguration'
  { -- | The configuration parameters for the default Amazon Glue database. You
    -- use this database for Apache Flink SQL queries and table API transforms
    -- that you write in a Kinesis Data Analytics Studio notebook.
    glueDataCatalogConfiguration :: GlueDataCatalogConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueDataCatalogConfiguration', 'catalogConfiguration_glueDataCatalogConfiguration' - The configuration parameters for the default Amazon Glue database. You
-- use this database for Apache Flink SQL queries and table API transforms
-- that you write in a Kinesis Data Analytics Studio notebook.
newCatalogConfiguration ::
  -- | 'glueDataCatalogConfiguration'
  GlueDataCatalogConfiguration ->
  CatalogConfiguration
newCatalogConfiguration
  pGlueDataCatalogConfiguration_ =
    CatalogConfiguration'
      { glueDataCatalogConfiguration =
          pGlueDataCatalogConfiguration_
      }

-- | The configuration parameters for the default Amazon Glue database. You
-- use this database for Apache Flink SQL queries and table API transforms
-- that you write in a Kinesis Data Analytics Studio notebook.
catalogConfiguration_glueDataCatalogConfiguration :: Lens.Lens' CatalogConfiguration GlueDataCatalogConfiguration
catalogConfiguration_glueDataCatalogConfiguration = Lens.lens (\CatalogConfiguration' {glueDataCatalogConfiguration} -> glueDataCatalogConfiguration) (\s@CatalogConfiguration' {} a -> s {glueDataCatalogConfiguration = a} :: CatalogConfiguration)

instance Prelude.Hashable CatalogConfiguration where
  hashWithSalt _salt CatalogConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` glueDataCatalogConfiguration

instance Prelude.NFData CatalogConfiguration where
  rnf CatalogConfiguration' {..} =
    Prelude.rnf glueDataCatalogConfiguration

instance Data.ToJSON CatalogConfiguration where
  toJSON CatalogConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "GlueDataCatalogConfiguration"
                  Data..= glueDataCatalogConfiguration
              )
          ]
      )
