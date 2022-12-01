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
-- Module      : Amazonka.SageMaker.Types.EdgeDeploymentConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeDeploymentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FailureHandlingPolicy

-- | Contains information about the configuration of a deployment.
--
-- /See:/ 'newEdgeDeploymentConfig' smart constructor.
data EdgeDeploymentConfig = EdgeDeploymentConfig'
  { -- | Toggle that determines whether to rollback to previous configuration if
    -- the current deployment fails. By default this is turned on. You may turn
    -- this off if you want to investigate the errors yourself.
    failureHandlingPolicy :: FailureHandlingPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureHandlingPolicy', 'edgeDeploymentConfig_failureHandlingPolicy' - Toggle that determines whether to rollback to previous configuration if
-- the current deployment fails. By default this is turned on. You may turn
-- this off if you want to investigate the errors yourself.
newEdgeDeploymentConfig ::
  -- | 'failureHandlingPolicy'
  FailureHandlingPolicy ->
  EdgeDeploymentConfig
newEdgeDeploymentConfig pFailureHandlingPolicy_ =
  EdgeDeploymentConfig'
    { failureHandlingPolicy =
        pFailureHandlingPolicy_
    }

-- | Toggle that determines whether to rollback to previous configuration if
-- the current deployment fails. By default this is turned on. You may turn
-- this off if you want to investigate the errors yourself.
edgeDeploymentConfig_failureHandlingPolicy :: Lens.Lens' EdgeDeploymentConfig FailureHandlingPolicy
edgeDeploymentConfig_failureHandlingPolicy = Lens.lens (\EdgeDeploymentConfig' {failureHandlingPolicy} -> failureHandlingPolicy) (\s@EdgeDeploymentConfig' {} a -> s {failureHandlingPolicy = a} :: EdgeDeploymentConfig)

instance Core.FromJSON EdgeDeploymentConfig where
  parseJSON =
    Core.withObject
      "EdgeDeploymentConfig"
      ( \x ->
          EdgeDeploymentConfig'
            Prelude.<$> (x Core..: "FailureHandlingPolicy")
      )

instance Prelude.Hashable EdgeDeploymentConfig where
  hashWithSalt _salt EdgeDeploymentConfig' {..} =
    _salt `Prelude.hashWithSalt` failureHandlingPolicy

instance Prelude.NFData EdgeDeploymentConfig where
  rnf EdgeDeploymentConfig' {..} =
    Prelude.rnf failureHandlingPolicy

instance Core.ToJSON EdgeDeploymentConfig where
  toJSON EdgeDeploymentConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FailureHandlingPolicy"
                  Core..= failureHandlingPolicy
              )
          ]
      )
