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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Updates to the configuration parameters for the default Amazon Glue
-- database. You use this database for SQL queries that you write in a
-- Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newCatalogConfigurationUpdate' smart constructor.
data CatalogConfigurationUpdate = CatalogConfigurationUpdate'
  { -- | Updates to the configuration parameters for the default Amazon Glue
    -- database. You use this database for SQL queries that you write in a
    -- Kinesis Data Analytics Studio notebook.
    glueDataCatalogConfigurationUpdate :: GlueDataCatalogConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueDataCatalogConfigurationUpdate', 'catalogConfigurationUpdate_glueDataCatalogConfigurationUpdate' - Updates to the configuration parameters for the default Amazon Glue
-- database. You use this database for SQL queries that you write in a
-- Kinesis Data Analytics Studio notebook.
newCatalogConfigurationUpdate ::
  -- | 'glueDataCatalogConfigurationUpdate'
  GlueDataCatalogConfigurationUpdate ->
  CatalogConfigurationUpdate
newCatalogConfigurationUpdate
  pGlueDataCatalogConfigurationUpdate_ =
    CatalogConfigurationUpdate'
      { glueDataCatalogConfigurationUpdate =
          pGlueDataCatalogConfigurationUpdate_
      }

-- | Updates to the configuration parameters for the default Amazon Glue
-- database. You use this database for SQL queries that you write in a
-- Kinesis Data Analytics Studio notebook.
catalogConfigurationUpdate_glueDataCatalogConfigurationUpdate :: Lens.Lens' CatalogConfigurationUpdate GlueDataCatalogConfigurationUpdate
catalogConfigurationUpdate_glueDataCatalogConfigurationUpdate = Lens.lens (\CatalogConfigurationUpdate' {glueDataCatalogConfigurationUpdate} -> glueDataCatalogConfigurationUpdate) (\s@CatalogConfigurationUpdate' {} a -> s {glueDataCatalogConfigurationUpdate = a} :: CatalogConfigurationUpdate)

instance Prelude.Hashable CatalogConfigurationUpdate where
  hashWithSalt _salt CatalogConfigurationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` glueDataCatalogConfigurationUpdate

instance Prelude.NFData CatalogConfigurationUpdate where
  rnf CatalogConfigurationUpdate' {..} =
    Prelude.rnf glueDataCatalogConfigurationUpdate

instance Data.ToJSON CatalogConfigurationUpdate where
  toJSON CatalogConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "GlueDataCatalogConfigurationUpdate"
                  Data..= glueDataCatalogConfigurationUpdate
              )
          ]
      )
