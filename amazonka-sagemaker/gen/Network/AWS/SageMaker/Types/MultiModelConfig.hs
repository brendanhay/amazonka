{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.MultiModelConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MultiModelConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ModelCacheSetting

-- | Specifies additional configuration for hosting multi-model endpoints.
--
-- /See:/ 'newMultiModelConfig' smart constructor.
data MultiModelConfig = MultiModelConfig'
  { -- | Whether to cache models for a multi-model endpoint. By default,
    -- multi-model endpoints cache models so that a model does not have to be
    -- loaded into memory each time it is invoked. Some use cases do not
    -- benefit from model caching. For example, if an endpoint hosts a large
    -- number of models that are each invoked infrequently, the endpoint might
    -- perform better if you disable model caching. To disable model caching,
    -- set the value of this parameter to @Disabled@.
    modelCacheSetting :: Prelude.Maybe ModelCacheSetting
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiModelConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelCacheSetting', 'multiModelConfig_modelCacheSetting' - Whether to cache models for a multi-model endpoint. By default,
-- multi-model endpoints cache models so that a model does not have to be
-- loaded into memory each time it is invoked. Some use cases do not
-- benefit from model caching. For example, if an endpoint hosts a large
-- number of models that are each invoked infrequently, the endpoint might
-- perform better if you disable model caching. To disable model caching,
-- set the value of this parameter to @Disabled@.
newMultiModelConfig ::
  MultiModelConfig
newMultiModelConfig =
  MultiModelConfig'
    { modelCacheSetting =
        Prelude.Nothing
    }

-- | Whether to cache models for a multi-model endpoint. By default,
-- multi-model endpoints cache models so that a model does not have to be
-- loaded into memory each time it is invoked. Some use cases do not
-- benefit from model caching. For example, if an endpoint hosts a large
-- number of models that are each invoked infrequently, the endpoint might
-- perform better if you disable model caching. To disable model caching,
-- set the value of this parameter to @Disabled@.
multiModelConfig_modelCacheSetting :: Lens.Lens' MultiModelConfig (Prelude.Maybe ModelCacheSetting)
multiModelConfig_modelCacheSetting = Lens.lens (\MultiModelConfig' {modelCacheSetting} -> modelCacheSetting) (\s@MultiModelConfig' {} a -> s {modelCacheSetting = a} :: MultiModelConfig)

instance Prelude.FromJSON MultiModelConfig where
  parseJSON =
    Prelude.withObject
      "MultiModelConfig"
      ( \x ->
          MultiModelConfig'
            Prelude.<$> (x Prelude..:? "ModelCacheSetting")
      )

instance Prelude.Hashable MultiModelConfig

instance Prelude.NFData MultiModelConfig

instance Prelude.ToJSON MultiModelConfig where
  toJSON MultiModelConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ModelCacheSetting" Prelude..=)
              Prelude.<$> modelCacheSetting
          ]
      )
