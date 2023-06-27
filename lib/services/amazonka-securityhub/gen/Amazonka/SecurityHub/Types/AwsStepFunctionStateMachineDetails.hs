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
-- Module      : Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineLoggingConfigurationDetails
import Amazonka.SecurityHub.Types.AwsStepFunctionStateMachineTracingConfigurationDetails

-- | Provides details about an Step Functions state machine, which is a
-- workflow consisting of a series of event- driven steps.
--
-- /See:/ 'newAwsStepFunctionStateMachineDetails' smart constructor.
data AwsStepFunctionStateMachineDetails = AwsStepFunctionStateMachineDetails'
  { -- | A user-defined or an auto-generated string that identifies a @Map@
    -- state. This parameter is present only if the @stateMachineArn@ specified
    -- in input is a qualified state machine ARN.
    label :: Prelude.Maybe Prelude.Text,
    -- | Used to set CloudWatch Logs options.
    loggingConfiguration :: Prelude.Maybe AwsStepFunctionStateMachineLoggingConfigurationDetails,
    -- | The name of the state machine.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used when creating this
    -- state machine.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN that identifies the state machine.
    stateMachineArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the state machine.
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether X-Ray tracing is enabled.
    tracingConfiguration :: Prelude.Maybe AwsStepFunctionStateMachineTracingConfigurationDetails,
    -- | The type of the state machine (STANDARD or EXPRESS).
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsStepFunctionStateMachineDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'awsStepFunctionStateMachineDetails_label' - A user-defined or an auto-generated string that identifies a @Map@
-- state. This parameter is present only if the @stateMachineArn@ specified
-- in input is a qualified state machine ARN.
--
-- 'loggingConfiguration', 'awsStepFunctionStateMachineDetails_loggingConfiguration' - Used to set CloudWatch Logs options.
--
-- 'name', 'awsStepFunctionStateMachineDetails_name' - The name of the state machine.
--
-- 'roleArn', 'awsStepFunctionStateMachineDetails_roleArn' - The Amazon Resource Name (ARN) of the IAM role used when creating this
-- state machine.
--
-- 'stateMachineArn', 'awsStepFunctionStateMachineDetails_stateMachineArn' - The ARN that identifies the state machine.
--
-- 'status', 'awsStepFunctionStateMachineDetails_status' - The current status of the state machine.
--
-- 'tracingConfiguration', 'awsStepFunctionStateMachineDetails_tracingConfiguration' - Specifies whether X-Ray tracing is enabled.
--
-- 'type'', 'awsStepFunctionStateMachineDetails_type' - The type of the state machine (STANDARD or EXPRESS).
newAwsStepFunctionStateMachineDetails ::
  AwsStepFunctionStateMachineDetails
newAwsStepFunctionStateMachineDetails =
  AwsStepFunctionStateMachineDetails'
    { label =
        Prelude.Nothing,
      loggingConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      stateMachineArn = Prelude.Nothing,
      status = Prelude.Nothing,
      tracingConfiguration = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A user-defined or an auto-generated string that identifies a @Map@
-- state. This parameter is present only if the @stateMachineArn@ specified
-- in input is a qualified state machine ARN.
awsStepFunctionStateMachineDetails_label :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_label = Lens.lens (\AwsStepFunctionStateMachineDetails' {label} -> label) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {label = a} :: AwsStepFunctionStateMachineDetails)

-- | Used to set CloudWatch Logs options.
awsStepFunctionStateMachineDetails_loggingConfiguration :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe AwsStepFunctionStateMachineLoggingConfigurationDetails)
awsStepFunctionStateMachineDetails_loggingConfiguration = Lens.lens (\AwsStepFunctionStateMachineDetails' {loggingConfiguration} -> loggingConfiguration) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {loggingConfiguration = a} :: AwsStepFunctionStateMachineDetails)

-- | The name of the state machine.
awsStepFunctionStateMachineDetails_name :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_name = Lens.lens (\AwsStepFunctionStateMachineDetails' {name} -> name) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {name = a} :: AwsStepFunctionStateMachineDetails)

-- | The Amazon Resource Name (ARN) of the IAM role used when creating this
-- state machine.
awsStepFunctionStateMachineDetails_roleArn :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_roleArn = Lens.lens (\AwsStepFunctionStateMachineDetails' {roleArn} -> roleArn) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {roleArn = a} :: AwsStepFunctionStateMachineDetails)

-- | The ARN that identifies the state machine.
awsStepFunctionStateMachineDetails_stateMachineArn :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_stateMachineArn = Lens.lens (\AwsStepFunctionStateMachineDetails' {stateMachineArn} -> stateMachineArn) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {stateMachineArn = a} :: AwsStepFunctionStateMachineDetails)

-- | The current status of the state machine.
awsStepFunctionStateMachineDetails_status :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_status = Lens.lens (\AwsStepFunctionStateMachineDetails' {status} -> status) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {status = a} :: AwsStepFunctionStateMachineDetails)

-- | Specifies whether X-Ray tracing is enabled.
awsStepFunctionStateMachineDetails_tracingConfiguration :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe AwsStepFunctionStateMachineTracingConfigurationDetails)
awsStepFunctionStateMachineDetails_tracingConfiguration = Lens.lens (\AwsStepFunctionStateMachineDetails' {tracingConfiguration} -> tracingConfiguration) (\s@AwsStepFunctionStateMachineDetails' {} a -> s {tracingConfiguration = a} :: AwsStepFunctionStateMachineDetails)

-- | The type of the state machine (STANDARD or EXPRESS).
awsStepFunctionStateMachineDetails_type :: Lens.Lens' AwsStepFunctionStateMachineDetails (Prelude.Maybe Prelude.Text)
awsStepFunctionStateMachineDetails_type = Lens.lens (\AwsStepFunctionStateMachineDetails' {type'} -> type') (\s@AwsStepFunctionStateMachineDetails' {} a -> s {type' = a} :: AwsStepFunctionStateMachineDetails)

instance
  Data.FromJSON
    AwsStepFunctionStateMachineDetails
  where
  parseJSON =
    Data.withObject
      "AwsStepFunctionStateMachineDetails"
      ( \x ->
          AwsStepFunctionStateMachineDetails'
            Prelude.<$> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "LoggingConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "StateMachineArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TracingConfiguration")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsStepFunctionStateMachineDetails
  where
  hashWithSalt
    _salt
    AwsStepFunctionStateMachineDetails' {..} =
      _salt
        `Prelude.hashWithSalt` label
        `Prelude.hashWithSalt` loggingConfiguration
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` stateMachineArn
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tracingConfiguration
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsStepFunctionStateMachineDetails
  where
  rnf AwsStepFunctionStateMachineDetails' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tracingConfiguration
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsStepFunctionStateMachineDetails
  where
  toJSON AwsStepFunctionStateMachineDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Label" Data..=) Prelude.<$> label,
            ("LoggingConfiguration" Data..=)
              Prelude.<$> loggingConfiguration,
            ("Name" Data..=) Prelude.<$> name,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("StateMachineArn" Data..=)
              Prelude.<$> stateMachineArn,
            ("Status" Data..=) Prelude.<$> status,
            ("TracingConfiguration" Data..=)
              Prelude.<$> tracingConfiguration,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
