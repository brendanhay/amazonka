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
-- Module      : Network.AWS.SMS.Types.SSMValidationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMValidationParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'newSSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { -- | The ID of the instance. The instance must have the following tag:
    -- UserForSMSApplicationValidation=true.
    instanceId :: Core.Maybe Core.Text,
    -- | The name of the S3 bucket for output.
    outputS3BucketName :: Core.Maybe Core.Text,
    -- | The location of the validation script.
    source :: Core.Maybe Source,
    -- | The type of validation script.
    scriptType :: Core.Maybe ScriptType,
    -- | The command to run the validation script
    command :: Core.Maybe Core.Text,
    -- | The timeout interval, in seconds.
    executionTimeoutSeconds :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SSMValidationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'sSMValidationParameters_instanceId' - The ID of the instance. The instance must have the following tag:
-- UserForSMSApplicationValidation=true.
--
-- 'outputS3BucketName', 'sSMValidationParameters_outputS3BucketName' - The name of the S3 bucket for output.
--
-- 'source', 'sSMValidationParameters_source' - The location of the validation script.
--
-- 'scriptType', 'sSMValidationParameters_scriptType' - The type of validation script.
--
-- 'command', 'sSMValidationParameters_command' - The command to run the validation script
--
-- 'executionTimeoutSeconds', 'sSMValidationParameters_executionTimeoutSeconds' - The timeout interval, in seconds.
newSSMValidationParameters ::
  SSMValidationParameters
newSSMValidationParameters =
  SSMValidationParameters'
    { instanceId = Core.Nothing,
      outputS3BucketName = Core.Nothing,
      source = Core.Nothing,
      scriptType = Core.Nothing,
      command = Core.Nothing,
      executionTimeoutSeconds = Core.Nothing
    }

-- | The ID of the instance. The instance must have the following tag:
-- UserForSMSApplicationValidation=true.
sSMValidationParameters_instanceId :: Lens.Lens' SSMValidationParameters (Core.Maybe Core.Text)
sSMValidationParameters_instanceId = Lens.lens (\SSMValidationParameters' {instanceId} -> instanceId) (\s@SSMValidationParameters' {} a -> s {instanceId = a} :: SSMValidationParameters)

-- | The name of the S3 bucket for output.
sSMValidationParameters_outputS3BucketName :: Lens.Lens' SSMValidationParameters (Core.Maybe Core.Text)
sSMValidationParameters_outputS3BucketName = Lens.lens (\SSMValidationParameters' {outputS3BucketName} -> outputS3BucketName) (\s@SSMValidationParameters' {} a -> s {outputS3BucketName = a} :: SSMValidationParameters)

-- | The location of the validation script.
sSMValidationParameters_source :: Lens.Lens' SSMValidationParameters (Core.Maybe Source)
sSMValidationParameters_source = Lens.lens (\SSMValidationParameters' {source} -> source) (\s@SSMValidationParameters' {} a -> s {source = a} :: SSMValidationParameters)

-- | The type of validation script.
sSMValidationParameters_scriptType :: Lens.Lens' SSMValidationParameters (Core.Maybe ScriptType)
sSMValidationParameters_scriptType = Lens.lens (\SSMValidationParameters' {scriptType} -> scriptType) (\s@SSMValidationParameters' {} a -> s {scriptType = a} :: SSMValidationParameters)

-- | The command to run the validation script
sSMValidationParameters_command :: Lens.Lens' SSMValidationParameters (Core.Maybe Core.Text)
sSMValidationParameters_command = Lens.lens (\SSMValidationParameters' {command} -> command) (\s@SSMValidationParameters' {} a -> s {command = a} :: SSMValidationParameters)

-- | The timeout interval, in seconds.
sSMValidationParameters_executionTimeoutSeconds :: Lens.Lens' SSMValidationParameters (Core.Maybe Core.Natural)
sSMValidationParameters_executionTimeoutSeconds = Lens.lens (\SSMValidationParameters' {executionTimeoutSeconds} -> executionTimeoutSeconds) (\s@SSMValidationParameters' {} a -> s {executionTimeoutSeconds = a} :: SSMValidationParameters)

instance Core.FromJSON SSMValidationParameters where
  parseJSON =
    Core.withObject
      "SSMValidationParameters"
      ( \x ->
          SSMValidationParameters'
            Core.<$> (x Core..:? "instanceId")
            Core.<*> (x Core..:? "outputS3BucketName")
            Core.<*> (x Core..:? "source")
            Core.<*> (x Core..:? "scriptType")
            Core.<*> (x Core..:? "command")
            Core.<*> (x Core..:? "executionTimeoutSeconds")
      )

instance Core.Hashable SSMValidationParameters

instance Core.NFData SSMValidationParameters

instance Core.ToJSON SSMValidationParameters where
  toJSON SSMValidationParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("instanceId" Core..=) Core.<$> instanceId,
            ("outputS3BucketName" Core..=)
              Core.<$> outputS3BucketName,
            ("source" Core..=) Core.<$> source,
            ("scriptType" Core..=) Core.<$> scriptType,
            ("command" Core..=) Core.<$> command,
            ("executionTimeoutSeconds" Core..=)
              Core.<$> executionTimeoutSeconds
          ]
      )
