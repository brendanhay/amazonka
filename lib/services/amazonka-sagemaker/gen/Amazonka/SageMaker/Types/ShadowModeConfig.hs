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
-- Module      : Amazonka.SageMaker.Types.ShadowModeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ShadowModeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ShadowModelVariantConfig

-- | The configuration of @ShadowMode@ inference experiment type, which
-- specifies a production variant to take all the inference requests, and a
-- shadow variant to which Amazon SageMaker replicates a percentage of the
-- inference requests. For the shadow variant it also specifies the
-- percentage of requests that Amazon SageMaker replicates.
--
-- /See:/ 'newShadowModeConfig' smart constructor.
data ShadowModeConfig = ShadowModeConfig'
  { -- | The name of the production variant, which takes all the inference
    -- requests.
    sourceModelVariantName :: Prelude.Text,
    -- | List of shadow variant configurations.
    shadowModelVariants :: Prelude.NonEmpty ShadowModelVariantConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShadowModeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceModelVariantName', 'shadowModeConfig_sourceModelVariantName' - The name of the production variant, which takes all the inference
-- requests.
--
-- 'shadowModelVariants', 'shadowModeConfig_shadowModelVariants' - List of shadow variant configurations.
newShadowModeConfig ::
  -- | 'sourceModelVariantName'
  Prelude.Text ->
  -- | 'shadowModelVariants'
  Prelude.NonEmpty ShadowModelVariantConfig ->
  ShadowModeConfig
newShadowModeConfig
  pSourceModelVariantName_
  pShadowModelVariants_ =
    ShadowModeConfig'
      { sourceModelVariantName =
          pSourceModelVariantName_,
        shadowModelVariants =
          Lens.coerced Lens.# pShadowModelVariants_
      }

-- | The name of the production variant, which takes all the inference
-- requests.
shadowModeConfig_sourceModelVariantName :: Lens.Lens' ShadowModeConfig Prelude.Text
shadowModeConfig_sourceModelVariantName = Lens.lens (\ShadowModeConfig' {sourceModelVariantName} -> sourceModelVariantName) (\s@ShadowModeConfig' {} a -> s {sourceModelVariantName = a} :: ShadowModeConfig)

-- | List of shadow variant configurations.
shadowModeConfig_shadowModelVariants :: Lens.Lens' ShadowModeConfig (Prelude.NonEmpty ShadowModelVariantConfig)
shadowModeConfig_shadowModelVariants = Lens.lens (\ShadowModeConfig' {shadowModelVariants} -> shadowModelVariants) (\s@ShadowModeConfig' {} a -> s {shadowModelVariants = a} :: ShadowModeConfig) Prelude.. Lens.coerced

instance Data.FromJSON ShadowModeConfig where
  parseJSON =
    Data.withObject
      "ShadowModeConfig"
      ( \x ->
          ShadowModeConfig'
            Prelude.<$> (x Data..: "SourceModelVariantName")
            Prelude.<*> (x Data..: "ShadowModelVariants")
      )

instance Prelude.Hashable ShadowModeConfig where
  hashWithSalt _salt ShadowModeConfig' {..} =
    _salt `Prelude.hashWithSalt` sourceModelVariantName
      `Prelude.hashWithSalt` shadowModelVariants

instance Prelude.NFData ShadowModeConfig where
  rnf ShadowModeConfig' {..} =
    Prelude.rnf sourceModelVariantName
      `Prelude.seq` Prelude.rnf shadowModelVariants

instance Data.ToJSON ShadowModeConfig where
  toJSON ShadowModeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SourceModelVariantName"
                  Data..= sourceModelVariantName
              ),
            Prelude.Just
              ("ShadowModelVariants" Data..= shadowModelVariants)
          ]
      )
