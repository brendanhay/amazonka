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
-- Module      : Amazonka.SageMaker.Types.ShadowModelVariantConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ShadowModelVariantConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name and sampling percentage of a shadow variant.
--
-- /See:/ 'newShadowModelVariantConfig' smart constructor.
data ShadowModelVariantConfig = ShadowModelVariantConfig'
  { -- | The name of the shadow variant.
    shadowModelVariantName :: Prelude.Text,
    -- | The percentage of inference requests that Amazon SageMaker replicates
    -- from the production variant to the shadow variant.
    samplingPercentage :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShadowModelVariantConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shadowModelVariantName', 'shadowModelVariantConfig_shadowModelVariantName' - The name of the shadow variant.
--
-- 'samplingPercentage', 'shadowModelVariantConfig_samplingPercentage' - The percentage of inference requests that Amazon SageMaker replicates
-- from the production variant to the shadow variant.
newShadowModelVariantConfig ::
  -- | 'shadowModelVariantName'
  Prelude.Text ->
  -- | 'samplingPercentage'
  Prelude.Int ->
  ShadowModelVariantConfig
newShadowModelVariantConfig
  pShadowModelVariantName_
  pSamplingPercentage_ =
    ShadowModelVariantConfig'
      { shadowModelVariantName =
          pShadowModelVariantName_,
        samplingPercentage = pSamplingPercentage_
      }

-- | The name of the shadow variant.
shadowModelVariantConfig_shadowModelVariantName :: Lens.Lens' ShadowModelVariantConfig Prelude.Text
shadowModelVariantConfig_shadowModelVariantName = Lens.lens (\ShadowModelVariantConfig' {shadowModelVariantName} -> shadowModelVariantName) (\s@ShadowModelVariantConfig' {} a -> s {shadowModelVariantName = a} :: ShadowModelVariantConfig)

-- | The percentage of inference requests that Amazon SageMaker replicates
-- from the production variant to the shadow variant.
shadowModelVariantConfig_samplingPercentage :: Lens.Lens' ShadowModelVariantConfig Prelude.Int
shadowModelVariantConfig_samplingPercentage = Lens.lens (\ShadowModelVariantConfig' {samplingPercentage} -> samplingPercentage) (\s@ShadowModelVariantConfig' {} a -> s {samplingPercentage = a} :: ShadowModelVariantConfig)

instance Data.FromJSON ShadowModelVariantConfig where
  parseJSON =
    Data.withObject
      "ShadowModelVariantConfig"
      ( \x ->
          ShadowModelVariantConfig'
            Prelude.<$> (x Data..: "ShadowModelVariantName")
            Prelude.<*> (x Data..: "SamplingPercentage")
      )

instance Prelude.Hashable ShadowModelVariantConfig where
  hashWithSalt _salt ShadowModelVariantConfig' {..} =
    _salt
      `Prelude.hashWithSalt` shadowModelVariantName
      `Prelude.hashWithSalt` samplingPercentage

instance Prelude.NFData ShadowModelVariantConfig where
  rnf ShadowModelVariantConfig' {..} =
    Prelude.rnf shadowModelVariantName
      `Prelude.seq` Prelude.rnf samplingPercentage

instance Data.ToJSON ShadowModelVariantConfig where
  toJSON ShadowModelVariantConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ShadowModelVariantName"
                  Data..= shadowModelVariantName
              ),
            Prelude.Just
              ("SamplingPercentage" Data..= samplingPercentage)
          ]
      )
