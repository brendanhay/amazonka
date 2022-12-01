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
-- Module      : Amazonka.AppFlow.Types.MetadataCatalogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MetadataCatalogConfig where

import Amazonka.AppFlow.Types.GlueDataCatalogConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- your data. When Amazon AppFlow catalogs your data, it stores metadata in
-- a data catalog.
--
-- /See:/ 'newMetadataCatalogConfig' smart constructor.
data MetadataCatalogConfig = MetadataCatalogConfig'
  { -- | Specifies the configuration that Amazon AppFlow uses when it catalogs
    -- your data with the Glue Data Catalog.
    glueDataCatalog :: Prelude.Maybe GlueDataCatalogConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataCatalogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueDataCatalog', 'metadataCatalogConfig_glueDataCatalog' - Specifies the configuration that Amazon AppFlow uses when it catalogs
-- your data with the Glue Data Catalog.
newMetadataCatalogConfig ::
  MetadataCatalogConfig
newMetadataCatalogConfig =
  MetadataCatalogConfig'
    { glueDataCatalog =
        Prelude.Nothing
    }

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- your data with the Glue Data Catalog.
metadataCatalogConfig_glueDataCatalog :: Lens.Lens' MetadataCatalogConfig (Prelude.Maybe GlueDataCatalogConfig)
metadataCatalogConfig_glueDataCatalog = Lens.lens (\MetadataCatalogConfig' {glueDataCatalog} -> glueDataCatalog) (\s@MetadataCatalogConfig' {} a -> s {glueDataCatalog = a} :: MetadataCatalogConfig)

instance Core.FromJSON MetadataCatalogConfig where
  parseJSON =
    Core.withObject
      "MetadataCatalogConfig"
      ( \x ->
          MetadataCatalogConfig'
            Prelude.<$> (x Core..:? "glueDataCatalog")
      )

instance Prelude.Hashable MetadataCatalogConfig where
  hashWithSalt _salt MetadataCatalogConfig' {..} =
    _salt `Prelude.hashWithSalt` glueDataCatalog

instance Prelude.NFData MetadataCatalogConfig where
  rnf MetadataCatalogConfig' {..} =
    Prelude.rnf glueDataCatalog

instance Core.ToJSON MetadataCatalogConfig where
  toJSON MetadataCatalogConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("glueDataCatalog" Core..=)
              Prelude.<$> glueDataCatalog
          ]
      )
