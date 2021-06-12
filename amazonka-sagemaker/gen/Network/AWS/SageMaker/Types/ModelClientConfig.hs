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
-- Module      : Network.AWS.SageMaker.Types.ModelClientConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelClientConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configures the timeout and maximum number of retries for processing a
-- transform job invocation.
--
-- /See:/ 'newModelClientConfig' smart constructor.
data ModelClientConfig = ModelClientConfig'
  { -- | The timeout value in seconds for an invocation request.
    invocationsTimeoutInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of retries when invocation requests are failing.
    invocationsMaxRetries :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelClientConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationsTimeoutInSeconds', 'modelClientConfig_invocationsTimeoutInSeconds' - The timeout value in seconds for an invocation request.
--
-- 'invocationsMaxRetries', 'modelClientConfig_invocationsMaxRetries' - The maximum number of retries when invocation requests are failing.
newModelClientConfig ::
  ModelClientConfig
newModelClientConfig =
  ModelClientConfig'
    { invocationsTimeoutInSeconds =
        Core.Nothing,
      invocationsMaxRetries = Core.Nothing
    }

-- | The timeout value in seconds for an invocation request.
modelClientConfig_invocationsTimeoutInSeconds :: Lens.Lens' ModelClientConfig (Core.Maybe Core.Natural)
modelClientConfig_invocationsTimeoutInSeconds = Lens.lens (\ModelClientConfig' {invocationsTimeoutInSeconds} -> invocationsTimeoutInSeconds) (\s@ModelClientConfig' {} a -> s {invocationsTimeoutInSeconds = a} :: ModelClientConfig)

-- | The maximum number of retries when invocation requests are failing.
modelClientConfig_invocationsMaxRetries :: Lens.Lens' ModelClientConfig (Core.Maybe Core.Natural)
modelClientConfig_invocationsMaxRetries = Lens.lens (\ModelClientConfig' {invocationsMaxRetries} -> invocationsMaxRetries) (\s@ModelClientConfig' {} a -> s {invocationsMaxRetries = a} :: ModelClientConfig)

instance Core.FromJSON ModelClientConfig where
  parseJSON =
    Core.withObject
      "ModelClientConfig"
      ( \x ->
          ModelClientConfig'
            Core.<$> (x Core..:? "InvocationsTimeoutInSeconds")
            Core.<*> (x Core..:? "InvocationsMaxRetries")
      )

instance Core.Hashable ModelClientConfig

instance Core.NFData ModelClientConfig

instance Core.ToJSON ModelClientConfig where
  toJSON ModelClientConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InvocationsTimeoutInSeconds" Core..=)
              Core.<$> invocationsTimeoutInSeconds,
            ("InvocationsMaxRetries" Core..=)
              Core.<$> invocationsMaxRetries
          ]
      )
