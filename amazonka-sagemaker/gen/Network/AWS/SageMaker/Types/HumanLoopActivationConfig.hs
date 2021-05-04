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
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig

-- | Provides information about how and under what conditions SageMaker
-- creates a human loop. If @HumanLoopActivationConfig@ is not given, then
-- all requests go to humans.
--
-- /See:/ 'newHumanLoopActivationConfig' smart constructor.
data HumanLoopActivationConfig = HumanLoopActivationConfig'
  { -- | Container structure for defining under what conditions SageMaker creates
    -- a human loop.
    humanLoopActivationConditionsConfig :: HumanLoopActivationConditionsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopActivationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopActivationConditionsConfig', 'humanLoopActivationConfig_humanLoopActivationConditionsConfig' - Container structure for defining under what conditions SageMaker creates
-- a human loop.
newHumanLoopActivationConfig ::
  -- | 'humanLoopActivationConditionsConfig'
  HumanLoopActivationConditionsConfig ->
  HumanLoopActivationConfig
newHumanLoopActivationConfig
  pHumanLoopActivationConditionsConfig_ =
    HumanLoopActivationConfig'
      { humanLoopActivationConditionsConfig =
          pHumanLoopActivationConditionsConfig_
      }

-- | Container structure for defining under what conditions SageMaker creates
-- a human loop.
humanLoopActivationConfig_humanLoopActivationConditionsConfig :: Lens.Lens' HumanLoopActivationConfig HumanLoopActivationConditionsConfig
humanLoopActivationConfig_humanLoopActivationConditionsConfig = Lens.lens (\HumanLoopActivationConfig' {humanLoopActivationConditionsConfig} -> humanLoopActivationConditionsConfig) (\s@HumanLoopActivationConfig' {} a -> s {humanLoopActivationConditionsConfig = a} :: HumanLoopActivationConfig)

instance Prelude.FromJSON HumanLoopActivationConfig where
  parseJSON =
    Prelude.withObject
      "HumanLoopActivationConfig"
      ( \x ->
          HumanLoopActivationConfig'
            Prelude.<$> (x Prelude..: "HumanLoopActivationConditionsConfig")
      )

instance Prelude.Hashable HumanLoopActivationConfig

instance Prelude.NFData HumanLoopActivationConfig

instance Prelude.ToJSON HumanLoopActivationConfig where
  toJSON HumanLoopActivationConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HumanLoopActivationConditionsConfig"
                  Prelude..= humanLoopActivationConditionsConfig
              )
          ]
      )
