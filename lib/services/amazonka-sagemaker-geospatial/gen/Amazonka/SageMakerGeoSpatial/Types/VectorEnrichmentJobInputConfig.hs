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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobInputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDocumentType

-- | The input structure for the InputConfig in a VectorEnrichmentJob.
--
-- /See:/ 'newVectorEnrichmentJobInputConfig' smart constructor.
data VectorEnrichmentJobInputConfig = VectorEnrichmentJobInputConfig'
  { dataSourceConfig :: VectorEnrichmentJobDataSourceConfigInput,
    documentType :: VectorEnrichmentJobDocumentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceConfig', 'vectorEnrichmentJobInputConfig_dataSourceConfig' -
--
-- 'documentType', 'vectorEnrichmentJobInputConfig_documentType' -
newVectorEnrichmentJobInputConfig ::
  -- | 'dataSourceConfig'
  VectorEnrichmentJobDataSourceConfigInput ->
  -- | 'documentType'
  VectorEnrichmentJobDocumentType ->
  VectorEnrichmentJobInputConfig
newVectorEnrichmentJobInputConfig
  pDataSourceConfig_
  pDocumentType_ =
    VectorEnrichmentJobInputConfig'
      { dataSourceConfig =
          pDataSourceConfig_,
        documentType = pDocumentType_
      }

vectorEnrichmentJobInputConfig_dataSourceConfig :: Lens.Lens' VectorEnrichmentJobInputConfig VectorEnrichmentJobDataSourceConfigInput
vectorEnrichmentJobInputConfig_dataSourceConfig = Lens.lens (\VectorEnrichmentJobInputConfig' {dataSourceConfig} -> dataSourceConfig) (\s@VectorEnrichmentJobInputConfig' {} a -> s {dataSourceConfig = a} :: VectorEnrichmentJobInputConfig)

vectorEnrichmentJobInputConfig_documentType :: Lens.Lens' VectorEnrichmentJobInputConfig VectorEnrichmentJobDocumentType
vectorEnrichmentJobInputConfig_documentType = Lens.lens (\VectorEnrichmentJobInputConfig' {documentType} -> documentType) (\s@VectorEnrichmentJobInputConfig' {} a -> s {documentType = a} :: VectorEnrichmentJobInputConfig)

instance Data.FromJSON VectorEnrichmentJobInputConfig where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobInputConfig"
      ( \x ->
          VectorEnrichmentJobInputConfig'
            Prelude.<$> (x Data..: "DataSourceConfig")
            Prelude.<*> (x Data..: "DocumentType")
      )

instance
  Prelude.Hashable
    VectorEnrichmentJobInputConfig
  where
  hashWithSalt
    _salt
    VectorEnrichmentJobInputConfig' {..} =
      _salt
        `Prelude.hashWithSalt` dataSourceConfig
        `Prelude.hashWithSalt` documentType

instance
  Prelude.NFData
    VectorEnrichmentJobInputConfig
  where
  rnf VectorEnrichmentJobInputConfig' {..} =
    Prelude.rnf dataSourceConfig
      `Prelude.seq` Prelude.rnf documentType

instance Data.ToJSON VectorEnrichmentJobInputConfig where
  toJSON VectorEnrichmentJobInputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSourceConfig" Data..= dataSourceConfig),
            Prelude.Just ("DocumentType" Data..= documentType)
          ]
      )
