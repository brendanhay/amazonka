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
-- Module      : Amazonka.IoTSiteWise.Types.TransformProcessingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.TransformProcessingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ComputeLocation
import Amazonka.IoTSiteWise.Types.ForwardingConfig
import qualified Amazonka.Prelude as Prelude

-- | The processing configuration for the given transform property. You can
-- configure transforms to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. You can also configure transforms to be computed at
-- the edge or in the cloud.
--
-- /See:/ 'newTransformProcessingConfig' smart constructor.
data TransformProcessingConfig = TransformProcessingConfig'
  { forwardingConfig :: Prelude.Maybe ForwardingConfig,
    -- | The compute location for the given transform property.
    computeLocation :: ComputeLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformProcessingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forwardingConfig', 'transformProcessingConfig_forwardingConfig' - Undocumented member.
--
-- 'computeLocation', 'transformProcessingConfig_computeLocation' - The compute location for the given transform property.
newTransformProcessingConfig ::
  -- | 'computeLocation'
  ComputeLocation ->
  TransformProcessingConfig
newTransformProcessingConfig pComputeLocation_ =
  TransformProcessingConfig'
    { forwardingConfig =
        Prelude.Nothing,
      computeLocation = pComputeLocation_
    }

-- | Undocumented member.
transformProcessingConfig_forwardingConfig :: Lens.Lens' TransformProcessingConfig (Prelude.Maybe ForwardingConfig)
transformProcessingConfig_forwardingConfig = Lens.lens (\TransformProcessingConfig' {forwardingConfig} -> forwardingConfig) (\s@TransformProcessingConfig' {} a -> s {forwardingConfig = a} :: TransformProcessingConfig)

-- | The compute location for the given transform property.
transformProcessingConfig_computeLocation :: Lens.Lens' TransformProcessingConfig ComputeLocation
transformProcessingConfig_computeLocation = Lens.lens (\TransformProcessingConfig' {computeLocation} -> computeLocation) (\s@TransformProcessingConfig' {} a -> s {computeLocation = a} :: TransformProcessingConfig)

instance Data.FromJSON TransformProcessingConfig where
  parseJSON =
    Data.withObject
      "TransformProcessingConfig"
      ( \x ->
          TransformProcessingConfig'
            Prelude.<$> (x Data..:? "forwardingConfig")
            Prelude.<*> (x Data..: "computeLocation")
      )

instance Prelude.Hashable TransformProcessingConfig where
  hashWithSalt _salt TransformProcessingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` forwardingConfig
      `Prelude.hashWithSalt` computeLocation

instance Prelude.NFData TransformProcessingConfig where
  rnf TransformProcessingConfig' {..} =
    Prelude.rnf forwardingConfig
      `Prelude.seq` Prelude.rnf computeLocation

instance Data.ToJSON TransformProcessingConfig where
  toJSON TransformProcessingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forwardingConfig" Data..=)
              Prelude.<$> forwardingConfig,
            Prelude.Just
              ("computeLocation" Data..= computeLocation)
          ]
      )
