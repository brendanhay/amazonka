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
-- Module      : Amazonka.SMS.Types.SSMValidationParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.SSMValidationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ScriptType
import Amazonka.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'newSSMValidationParameters' smart constructor.
data SSMValidationParameters = SSMValidationParameters'
  { -- | The command to run the validation script.
    command :: Prelude.Maybe Prelude.Text,
    -- | The timeout interval, in seconds.
    executionTimeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the instance. The instance must have the following tag:
    -- UserForSMSApplicationValidation=true.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket for output.
    outputS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The type of validation script.
    scriptType :: Prelude.Maybe ScriptType,
    -- | The location of the validation script.
    source :: Prelude.Maybe Source
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
-- 'command', 'sSMValidationParameters_command' - The command to run the validation script.
--
-- 'executionTimeoutSeconds', 'sSMValidationParameters_executionTimeoutSeconds' - The timeout interval, in seconds.
--
-- 'instanceId', 'sSMValidationParameters_instanceId' - The ID of the instance. The instance must have the following tag:
-- UserForSMSApplicationValidation=true.
--
-- 'outputS3BucketName', 'sSMValidationParameters_outputS3BucketName' - The name of the S3 bucket for output.
--
-- 'scriptType', 'sSMValidationParameters_scriptType' - The type of validation script.
--
-- 'source', 'sSMValidationParameters_source' - The location of the validation script.
newSSMValidationParameters ::
  SSMValidationParameters
newSSMValidationParameters =
  SSMValidationParameters'
    { command = Prelude.Nothing,
      executionTimeoutSeconds = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      outputS3BucketName = Prelude.Nothing,
      scriptType = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The command to run the validation script.
sSMValidationParameters_command :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_command = Lens.lens (\SSMValidationParameters' {command} -> command) (\s@SSMValidationParameters' {} a -> s {command = a} :: SSMValidationParameters)

-- | The timeout interval, in seconds.
sSMValidationParameters_executionTimeoutSeconds :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Natural)
sSMValidationParameters_executionTimeoutSeconds = Lens.lens (\SSMValidationParameters' {executionTimeoutSeconds} -> executionTimeoutSeconds) (\s@SSMValidationParameters' {} a -> s {executionTimeoutSeconds = a} :: SSMValidationParameters)

-- | The ID of the instance. The instance must have the following tag:
-- UserForSMSApplicationValidation=true.
sSMValidationParameters_instanceId :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_instanceId = Lens.lens (\SSMValidationParameters' {instanceId} -> instanceId) (\s@SSMValidationParameters' {} a -> s {instanceId = a} :: SSMValidationParameters)

-- | The name of the S3 bucket for output.
sSMValidationParameters_outputS3BucketName :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Prelude.Text)
sSMValidationParameters_outputS3BucketName = Lens.lens (\SSMValidationParameters' {outputS3BucketName} -> outputS3BucketName) (\s@SSMValidationParameters' {} a -> s {outputS3BucketName = a} :: SSMValidationParameters)

-- | The type of validation script.
sSMValidationParameters_scriptType :: Lens.Lens' SSMValidationParameters (Prelude.Maybe ScriptType)
sSMValidationParameters_scriptType = Lens.lens (\SSMValidationParameters' {scriptType} -> scriptType) (\s@SSMValidationParameters' {} a -> s {scriptType = a} :: SSMValidationParameters)

-- | The location of the validation script.
sSMValidationParameters_source :: Lens.Lens' SSMValidationParameters (Prelude.Maybe Source)
sSMValidationParameters_source = Lens.lens (\SSMValidationParameters' {source} -> source) (\s@SSMValidationParameters' {} a -> s {source = a} :: SSMValidationParameters)

instance Data.FromJSON SSMValidationParameters where
  parseJSON =
    Data.withObject
      "SSMValidationParameters"
      ( \x ->
          SSMValidationParameters'
            Prelude.<$> (x Data..:? "command")
            Prelude.<*> (x Data..:? "executionTimeoutSeconds")
            Prelude.<*> (x Data..:? "instanceId")
            Prelude.<*> (x Data..:? "outputS3BucketName")
            Prelude.<*> (x Data..:? "scriptType")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable SSMValidationParameters where
  hashWithSalt _salt SSMValidationParameters' {..} =
    _salt `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` executionTimeoutSeconds
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` outputS3BucketName
      `Prelude.hashWithSalt` scriptType
      `Prelude.hashWithSalt` source

instance Prelude.NFData SSMValidationParameters where
  rnf SSMValidationParameters' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf executionTimeoutSeconds
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf outputS3BucketName
      `Prelude.seq` Prelude.rnf scriptType
      `Prelude.seq` Prelude.rnf source

instance Data.ToJSON SSMValidationParameters where
  toJSON SSMValidationParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("command" Data..=) Prelude.<$> command,
            ("executionTimeoutSeconds" Data..=)
              Prelude.<$> executionTimeoutSeconds,
            ("instanceId" Data..=) Prelude.<$> instanceId,
            ("outputS3BucketName" Data..=)
              Prelude.<$> outputS3BucketName,
            ("scriptType" Data..=) Prelude.<$> scriptType,
            ("source" Data..=) Prelude.<$> source
          ]
      )
