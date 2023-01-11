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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.MapMatchingConfig
import Amazonka.SageMakerGeoSpatial.Types.ReverseGeocodingConfig

-- | It contains configs such as ReverseGeocodingConfig and
-- MapMatchingConfig.
--
-- /See:/ 'newVectorEnrichmentJobConfig' smart constructor.
data VectorEnrichmentJobConfig = VectorEnrichmentJobConfig'
  { mapMatchingConfig :: Prelude.Maybe MapMatchingConfig,
    reverseGeocodingConfig :: Prelude.Maybe ReverseGeocodingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapMatchingConfig', 'vectorEnrichmentJobConfig_mapMatchingConfig' -
--
-- 'reverseGeocodingConfig', 'vectorEnrichmentJobConfig_reverseGeocodingConfig' -
newVectorEnrichmentJobConfig ::
  VectorEnrichmentJobConfig
newVectorEnrichmentJobConfig =
  VectorEnrichmentJobConfig'
    { mapMatchingConfig =
        Prelude.Nothing,
      reverseGeocodingConfig = Prelude.Nothing
    }

-- |
vectorEnrichmentJobConfig_mapMatchingConfig :: Lens.Lens' VectorEnrichmentJobConfig (Prelude.Maybe MapMatchingConfig)
vectorEnrichmentJobConfig_mapMatchingConfig = Lens.lens (\VectorEnrichmentJobConfig' {mapMatchingConfig} -> mapMatchingConfig) (\s@VectorEnrichmentJobConfig' {} a -> s {mapMatchingConfig = a} :: VectorEnrichmentJobConfig)

-- |
vectorEnrichmentJobConfig_reverseGeocodingConfig :: Lens.Lens' VectorEnrichmentJobConfig (Prelude.Maybe ReverseGeocodingConfig)
vectorEnrichmentJobConfig_reverseGeocodingConfig = Lens.lens (\VectorEnrichmentJobConfig' {reverseGeocodingConfig} -> reverseGeocodingConfig) (\s@VectorEnrichmentJobConfig' {} a -> s {reverseGeocodingConfig = a} :: VectorEnrichmentJobConfig)

instance Data.FromJSON VectorEnrichmentJobConfig where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobConfig"
      ( \x ->
          VectorEnrichmentJobConfig'
            Prelude.<$> (x Data..:? "MapMatchingConfig")
            Prelude.<*> (x Data..:? "ReverseGeocodingConfig")
      )

instance Prelude.Hashable VectorEnrichmentJobConfig where
  hashWithSalt _salt VectorEnrichmentJobConfig' {..} =
    _salt `Prelude.hashWithSalt` mapMatchingConfig
      `Prelude.hashWithSalt` reverseGeocodingConfig

instance Prelude.NFData VectorEnrichmentJobConfig where
  rnf VectorEnrichmentJobConfig' {..} =
    Prelude.rnf mapMatchingConfig
      `Prelude.seq` Prelude.rnf reverseGeocodingConfig

instance Data.ToJSON VectorEnrichmentJobConfig where
  toJSON VectorEnrichmentJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MapMatchingConfig" Data..=)
              Prelude.<$> mapMatchingConfig,
            ("ReverseGeocodingConfig" Data..=)
              Prelude.<$> reverseGeocodingConfig
          ]
      )
