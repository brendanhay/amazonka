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
import qualified Network.AWS.Prelude as Prelude

-- | Configures the timeout and maximum number of retries for processing a
-- transform job invocation.
--
-- /See:/ 'newModelClientConfig' smart constructor.
data ModelClientConfig = ModelClientConfig'
  { -- | The timeout value in seconds for an invocation request.
    invocationsTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of retries when invocation requests are failing.
    invocationsMaxRetries :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      invocationsMaxRetries = Prelude.Nothing
    }

-- | The timeout value in seconds for an invocation request.
modelClientConfig_invocationsTimeoutInSeconds :: Lens.Lens' ModelClientConfig (Prelude.Maybe Prelude.Natural)
modelClientConfig_invocationsTimeoutInSeconds = Lens.lens (\ModelClientConfig' {invocationsTimeoutInSeconds} -> invocationsTimeoutInSeconds) (\s@ModelClientConfig' {} a -> s {invocationsTimeoutInSeconds = a} :: ModelClientConfig)

-- | The maximum number of retries when invocation requests are failing.
modelClientConfig_invocationsMaxRetries :: Lens.Lens' ModelClientConfig (Prelude.Maybe Prelude.Natural)
modelClientConfig_invocationsMaxRetries = Lens.lens (\ModelClientConfig' {invocationsMaxRetries} -> invocationsMaxRetries) (\s@ModelClientConfig' {} a -> s {invocationsMaxRetries = a} :: ModelClientConfig)

instance Core.FromJSON ModelClientConfig where
  parseJSON =
    Core.withObject
      "ModelClientConfig"
      ( \x ->
          ModelClientConfig'
            Prelude.<$> (x Core..:? "InvocationsTimeoutInSeconds")
            Prelude.<*> (x Core..:? "InvocationsMaxRetries")
      )

instance Prelude.Hashable ModelClientConfig

instance Prelude.NFData ModelClientConfig

instance Core.ToJSON ModelClientConfig where
  toJSON ModelClientConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InvocationsTimeoutInSeconds" Core..=)
              Prelude.<$> invocationsTimeoutInSeconds,
            ("InvocationsMaxRetries" Core..=)
              Prelude.<$> invocationsMaxRetries
          ]
      )
