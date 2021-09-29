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
-- Module      : Network.AWS.ECS.Types.ExecuteCommandConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ExecuteCommandConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.ExecuteCommandLogConfiguration
import Network.AWS.ECS.Types.ExecuteCommandLogging
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the execute command configuration.
--
-- /See:/ 'newExecuteCommandConfiguration' smart constructor.
data ExecuteCommandConfiguration = ExecuteCommandConfiguration'
  { -- | The log setting to use for redirecting logs for your execute command
    -- results. The following log settings are available.
    --
    -- -   @NONE@: The execute command session is not logged.
    --
    -- -   @DEFAULT@: The @awslogs@ configuration in the task definition is
    --     used. If no logging parameter is specified, it defaults to this
    --     value. If no @awslogs@ log driver is configured in the task
    --     definition, the output won\'t be logged.
    --
    -- -   @OVERRIDE@: Specify the logging details as a part of
    --     @logConfiguration@. If the @OVERRIDE@ logging option is specified,
    --     the @logConfiguration@ is required.
    logging :: Prelude.Maybe ExecuteCommandLogging,
    -- | Specify an Key Management Service key ID to encrypt the data between the
    -- local client and the container.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The log configuration for the results of the execute command actions.
    -- The logs can be sent to CloudWatch Logs or an Amazon S3 bucket. When
    -- @logging=OVERRIDE@ is specified, a @logConfiguration@ must be provided.
    logConfiguration :: Prelude.Maybe ExecuteCommandLogConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCommandConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logging', 'executeCommandConfiguration_logging' - The log setting to use for redirecting logs for your execute command
-- results. The following log settings are available.
--
-- -   @NONE@: The execute command session is not logged.
--
-- -   @DEFAULT@: The @awslogs@ configuration in the task definition is
--     used. If no logging parameter is specified, it defaults to this
--     value. If no @awslogs@ log driver is configured in the task
--     definition, the output won\'t be logged.
--
-- -   @OVERRIDE@: Specify the logging details as a part of
--     @logConfiguration@. If the @OVERRIDE@ logging option is specified,
--     the @logConfiguration@ is required.
--
-- 'kmsKeyId', 'executeCommandConfiguration_kmsKeyId' - Specify an Key Management Service key ID to encrypt the data between the
-- local client and the container.
--
-- 'logConfiguration', 'executeCommandConfiguration_logConfiguration' - The log configuration for the results of the execute command actions.
-- The logs can be sent to CloudWatch Logs or an Amazon S3 bucket. When
-- @logging=OVERRIDE@ is specified, a @logConfiguration@ must be provided.
newExecuteCommandConfiguration ::
  ExecuteCommandConfiguration
newExecuteCommandConfiguration =
  ExecuteCommandConfiguration'
    { logging =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logConfiguration = Prelude.Nothing
    }

-- | The log setting to use for redirecting logs for your execute command
-- results. The following log settings are available.
--
-- -   @NONE@: The execute command session is not logged.
--
-- -   @DEFAULT@: The @awslogs@ configuration in the task definition is
--     used. If no logging parameter is specified, it defaults to this
--     value. If no @awslogs@ log driver is configured in the task
--     definition, the output won\'t be logged.
--
-- -   @OVERRIDE@: Specify the logging details as a part of
--     @logConfiguration@. If the @OVERRIDE@ logging option is specified,
--     the @logConfiguration@ is required.
executeCommandConfiguration_logging :: Lens.Lens' ExecuteCommandConfiguration (Prelude.Maybe ExecuteCommandLogging)
executeCommandConfiguration_logging = Lens.lens (\ExecuteCommandConfiguration' {logging} -> logging) (\s@ExecuteCommandConfiguration' {} a -> s {logging = a} :: ExecuteCommandConfiguration)

-- | Specify an Key Management Service key ID to encrypt the data between the
-- local client and the container.
executeCommandConfiguration_kmsKeyId :: Lens.Lens' ExecuteCommandConfiguration (Prelude.Maybe Prelude.Text)
executeCommandConfiguration_kmsKeyId = Lens.lens (\ExecuteCommandConfiguration' {kmsKeyId} -> kmsKeyId) (\s@ExecuteCommandConfiguration' {} a -> s {kmsKeyId = a} :: ExecuteCommandConfiguration)

-- | The log configuration for the results of the execute command actions.
-- The logs can be sent to CloudWatch Logs or an Amazon S3 bucket. When
-- @logging=OVERRIDE@ is specified, a @logConfiguration@ must be provided.
executeCommandConfiguration_logConfiguration :: Lens.Lens' ExecuteCommandConfiguration (Prelude.Maybe ExecuteCommandLogConfiguration)
executeCommandConfiguration_logConfiguration = Lens.lens (\ExecuteCommandConfiguration' {logConfiguration} -> logConfiguration) (\s@ExecuteCommandConfiguration' {} a -> s {logConfiguration = a} :: ExecuteCommandConfiguration)

instance Core.FromJSON ExecuteCommandConfiguration where
  parseJSON =
    Core.withObject
      "ExecuteCommandConfiguration"
      ( \x ->
          ExecuteCommandConfiguration'
            Prelude.<$> (x Core..:? "logging")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "logConfiguration")
      )

instance Prelude.Hashable ExecuteCommandConfiguration

instance Prelude.NFData ExecuteCommandConfiguration

instance Core.ToJSON ExecuteCommandConfiguration where
  toJSON ExecuteCommandConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("logging" Core..=) Prelude.<$> logging,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("logConfiguration" Core..=)
              Prelude.<$> logConfiguration
          ]
      )
