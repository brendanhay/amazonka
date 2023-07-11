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
-- Module      : Amazonka.SageMaker.Types.HumanLoopActivationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HumanLoopActivationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON HumanLoopActivationConfig where
  parseJSON =
    Data.withObject
      "HumanLoopActivationConfig"
      ( \x ->
          HumanLoopActivationConfig'
            Prelude.<$> (x Data..: "HumanLoopActivationConditionsConfig")
      )

instance Prelude.Hashable HumanLoopActivationConfig where
  hashWithSalt _salt HumanLoopActivationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` humanLoopActivationConditionsConfig

instance Prelude.NFData HumanLoopActivationConfig where
  rnf HumanLoopActivationConfig' {..} =
    Prelude.rnf humanLoopActivationConditionsConfig

instance Data.ToJSON HumanLoopActivationConfig where
  toJSON HumanLoopActivationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HumanLoopActivationConditionsConfig"
                  Data..= humanLoopActivationConditionsConfig
              )
          ]
      )
