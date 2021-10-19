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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'newSSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { -- | The ID of the instance. The instance must have the following tag:
    -- UserForSMSApplicationValidation=true.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The command to run the validation script
    command :: Prelude.Maybe Prelude.Text,
    -- | The timeout interval, in seconds.
    executionTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The type of validation script.
    scriptType :: Prelude.Maybe ScriptType,
    -- | The location of the validation script.
    source :: Prelude.Maybe Source,
    -- | The name of the S3 bucket for output.
    outputS3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'command', 'sSMValidationParameters_command' - The command to run the validation script
--
-- 'executionTimeoutSeconds', 'sSMValidationParameters_executionTimeoutSeconds' - The timeout interval, in seconds.
--
-- 'scriptType', 'sSMValidationParameters_scriptType' - The type of validation script.
--
-- 'source', 'sSMValidationParameters_source' - The location of the validation script.
--
-- 'outputS3BucketName', 'sSMValidationParameters_outputS3BucketName' - The name of the S3 bucket for output.
newSSMValidationParameters ::
  SSMValidationParameters
newSSMValidationParameters =
  SSMValidationParameters'
    { instanceId =
        Prelude.Nothing,
      command = Prelude.Nothing,
      executionTimeoutSeconds = Prelude.Nothing,
      scriptType = Prelude.Nothing,
      source = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing
    }

-- | The ID of the instance. The instance must have the following tag:
-- UserForSMSApplicationValidation=true.
sSMValidationParameters_instanceId :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_instanceId = Lens.lens (\SSMValidationParameters' {instanceId} -> instanceId) (\s@SSMValidationParameters' {} a -> s {instanceId = a} :: SSMValidationParameters)

-- | The command to run the validation script
sSMValidationParameters_command :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_command = Lens.lens (\SSMValidationParameters' {command} -> command) (\s@SSMValidationParameters' {} a -> s {command = a} :: SSMValidationParameters)

-- | The timeout interval, in seconds.
sSMValidationParameters_executionTimeoutSeconds :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Natural)
sSMValidationParameters_executionTimeoutSeconds = Lens.lens (\SSMValidationParameters' {executionTimeoutSeconds} -> executionTimeoutSeconds) (\s@SSMValidationParameters' {} a -> s {executionTimeoutSeconds = a} :: SSMValidationParameters)

-- | The type of validation script.
sSMValidationParameters_scriptType :: Lens.Lens' SSMValidationParameters (Prelude.Maybe ScriptType)
sSMValidationParameters_scriptType = Lens.lens (\SSMValidationParameters' {scriptType} -> scriptType) (\s@SSMValidationParameters' {} a -> s {scriptType = a} :: SSMValidationParameters)

-- | The location of the validation script.
sSMValidationParameters_source :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Source)
sSMValidationParameters_source = Lens.lens (\SSMValidationParameters' {source} -> source) (\s@SSMValidationParameters' {} a -> s {source = a} :: SSMValidationParameters)

-- | The name of the S3 bucket for output.
sSMValidationParameters_outputS3BucketName :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_outputS3BucketName = Lens.lens (\SSMValidationParameters' {outputS3BucketName} -> outputS3BucketName) (\s@SSMValidationParameters' {} a -> s {outputS3BucketName = a} :: SSMValidationParameters)

instance Core.FromJSON SSMValidationParameters where
  parseJSON =
    Core.withObject
      "SSMValidationParameters"
      ( \x ->
          SSMValidationParameters'
            Prelude.<$> (x Core..:? "instanceId")
            Prelude.<*> (x Core..:? "command")
            Prelude.<*> (x Core..:? "executionTimeoutSeconds")
            Prelude.<*> (x Core..:? "scriptType")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "outputS3BucketName")
      )

instance Prelude.Hashable SSMValidationParameters

instance Prelude.NFData SSMValidationParameters

instance Core.ToJSON SSMValidationParameters where
  toJSON SSMValidationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("instanceId" Core..=) Prelude.<$> instanceId,
            ("command" Core..=) Prelude.<$> command,
            ("executionTimeoutSeconds" Core..=)
              Prelude.<$> executionTimeoutSeconds,
            ("scriptType" Core..=) Prelude.<$> scriptType,
            ("source" Core..=) Prelude.<$> source,
            ("outputS3BucketName" Core..=)
              Prelude.<$> outputS3BucketName
          ]
      )
