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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFormationStackDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFormationStackDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFormationStackDriftInformationDetails
import Amazonka.SecurityHub.Types.AwsCloudFormationStackOutputsDetails

-- | Nests a stack as a resource in a top-level template. Nested stacks are
-- stacks created as resources for another stack.
--
-- /See:/ 'newAwsCloudFormationStackDetails' smart constructor.
data AwsCloudFormationStackDetails = AwsCloudFormationStackDetails'
  { -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an IAM role that\'s associated with the stack.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The length of time, in minutes, that CloudFormation waits for the nested
    -- stack to reach the @CREATE_COMPLETE@ state.
    timeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Whether termination protection is enabled for the stack.
    enableTerminationProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of the Amazon SNS topic to which
    -- stack-related events are published.
    notificationArns :: Prelude.Maybe [Prelude.Text],
    -- | Success or failure message associated with the stack status.
    stackStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Boolean to enable or disable rollback on stack creation failures.
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | The time the nested stack was last updated. This field will only be
    -- returned if the stack has been updated at least once.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | A user-defined description associated with the stack.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name associated with the stack.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Current status of the stack.
    stackStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of output structures.
    outputs :: Prelude.Maybe [AwsCloudFormationStackOutputsDetails],
    -- | The capabilities allowed in the stack.
    capabilities :: Prelude.Maybe [Prelude.Text],
    -- | The time at which the stack was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | Information about whether a stack\'s actual configuration differs, or
    -- has drifted, from its expected configuration, as defined in the stack
    -- template and any values specified as template parameters.
    driftInformation :: Prelude.Maybe AwsCloudFormationStackDriftInformationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFormationStackDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'awsCloudFormationStackDetails_stackId' - Unique identifier of the stack.
--
-- 'roleArn', 'awsCloudFormationStackDetails_roleArn' - The ARN of an IAM role that\'s associated with the stack.
--
-- 'timeoutInMinutes', 'awsCloudFormationStackDetails_timeoutInMinutes' - The length of time, in minutes, that CloudFormation waits for the nested
-- stack to reach the @CREATE_COMPLETE@ state.
--
-- 'enableTerminationProtection', 'awsCloudFormationStackDetails_enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- 'notificationArns', 'awsCloudFormationStackDetails_notificationArns' - The Amazon Resource Names (ARNs) of the Amazon SNS topic to which
-- stack-related events are published.
--
-- 'stackStatusReason', 'awsCloudFormationStackDetails_stackStatusReason' - Success or failure message associated with the stack status.
--
-- 'disableRollback', 'awsCloudFormationStackDetails_disableRollback' - Boolean to enable or disable rollback on stack creation failures.
--
-- 'lastUpdatedTime', 'awsCloudFormationStackDetails_lastUpdatedTime' - The time the nested stack was last updated. This field will only be
-- returned if the stack has been updated at least once.
--
-- 'description', 'awsCloudFormationStackDetails_description' - A user-defined description associated with the stack.
--
-- 'stackName', 'awsCloudFormationStackDetails_stackName' - The name associated with the stack.
--
-- 'stackStatus', 'awsCloudFormationStackDetails_stackStatus' - Current status of the stack.
--
-- 'outputs', 'awsCloudFormationStackDetails_outputs' - A list of output structures.
--
-- 'capabilities', 'awsCloudFormationStackDetails_capabilities' - The capabilities allowed in the stack.
--
-- 'creationTime', 'awsCloudFormationStackDetails_creationTime' - The time at which the stack was created.
--
-- 'driftInformation', 'awsCloudFormationStackDetails_driftInformation' - Information about whether a stack\'s actual configuration differs, or
-- has drifted, from its expected configuration, as defined in the stack
-- template and any values specified as template parameters.
newAwsCloudFormationStackDetails ::
  AwsCloudFormationStackDetails
newAwsCloudFormationStackDetails =
  AwsCloudFormationStackDetails'
    { stackId =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      enableTerminationProtection =
        Prelude.Nothing,
      notificationArns = Prelude.Nothing,
      stackStatusReason = Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      description = Prelude.Nothing,
      stackName = Prelude.Nothing,
      stackStatus = Prelude.Nothing,
      outputs = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      driftInformation = Prelude.Nothing
    }

-- | Unique identifier of the stack.
awsCloudFormationStackDetails_stackId :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_stackId = Lens.lens (\AwsCloudFormationStackDetails' {stackId} -> stackId) (\s@AwsCloudFormationStackDetails' {} a -> s {stackId = a} :: AwsCloudFormationStackDetails)

-- | The ARN of an IAM role that\'s associated with the stack.
awsCloudFormationStackDetails_roleArn :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_roleArn = Lens.lens (\AwsCloudFormationStackDetails' {roleArn} -> roleArn) (\s@AwsCloudFormationStackDetails' {} a -> s {roleArn = a} :: AwsCloudFormationStackDetails)

-- | The length of time, in minutes, that CloudFormation waits for the nested
-- stack to reach the @CREATE_COMPLETE@ state.
awsCloudFormationStackDetails_timeoutInMinutes :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Int)
awsCloudFormationStackDetails_timeoutInMinutes = Lens.lens (\AwsCloudFormationStackDetails' {timeoutInMinutes} -> timeoutInMinutes) (\s@AwsCloudFormationStackDetails' {} a -> s {timeoutInMinutes = a} :: AwsCloudFormationStackDetails)

-- | Whether termination protection is enabled for the stack.
awsCloudFormationStackDetails_enableTerminationProtection :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Bool)
awsCloudFormationStackDetails_enableTerminationProtection = Lens.lens (\AwsCloudFormationStackDetails' {enableTerminationProtection} -> enableTerminationProtection) (\s@AwsCloudFormationStackDetails' {} a -> s {enableTerminationProtection = a} :: AwsCloudFormationStackDetails)

-- | The Amazon Resource Names (ARNs) of the Amazon SNS topic to which
-- stack-related events are published.
awsCloudFormationStackDetails_notificationArns :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe [Prelude.Text])
awsCloudFormationStackDetails_notificationArns = Lens.lens (\AwsCloudFormationStackDetails' {notificationArns} -> notificationArns) (\s@AwsCloudFormationStackDetails' {} a -> s {notificationArns = a} :: AwsCloudFormationStackDetails) Prelude.. Lens.mapping Lens.coerced

-- | Success or failure message associated with the stack status.
awsCloudFormationStackDetails_stackStatusReason :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_stackStatusReason = Lens.lens (\AwsCloudFormationStackDetails' {stackStatusReason} -> stackStatusReason) (\s@AwsCloudFormationStackDetails' {} a -> s {stackStatusReason = a} :: AwsCloudFormationStackDetails)

-- | Boolean to enable or disable rollback on stack creation failures.
awsCloudFormationStackDetails_disableRollback :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Bool)
awsCloudFormationStackDetails_disableRollback = Lens.lens (\AwsCloudFormationStackDetails' {disableRollback} -> disableRollback) (\s@AwsCloudFormationStackDetails' {} a -> s {disableRollback = a} :: AwsCloudFormationStackDetails)

-- | The time the nested stack was last updated. This field will only be
-- returned if the stack has been updated at least once.
awsCloudFormationStackDetails_lastUpdatedTime :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_lastUpdatedTime = Lens.lens (\AwsCloudFormationStackDetails' {lastUpdatedTime} -> lastUpdatedTime) (\s@AwsCloudFormationStackDetails' {} a -> s {lastUpdatedTime = a} :: AwsCloudFormationStackDetails)

-- | A user-defined description associated with the stack.
awsCloudFormationStackDetails_description :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_description = Lens.lens (\AwsCloudFormationStackDetails' {description} -> description) (\s@AwsCloudFormationStackDetails' {} a -> s {description = a} :: AwsCloudFormationStackDetails)

-- | The name associated with the stack.
awsCloudFormationStackDetails_stackName :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_stackName = Lens.lens (\AwsCloudFormationStackDetails' {stackName} -> stackName) (\s@AwsCloudFormationStackDetails' {} a -> s {stackName = a} :: AwsCloudFormationStackDetails)

-- | Current status of the stack.
awsCloudFormationStackDetails_stackStatus :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_stackStatus = Lens.lens (\AwsCloudFormationStackDetails' {stackStatus} -> stackStatus) (\s@AwsCloudFormationStackDetails' {} a -> s {stackStatus = a} :: AwsCloudFormationStackDetails)

-- | A list of output structures.
awsCloudFormationStackDetails_outputs :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe [AwsCloudFormationStackOutputsDetails])
awsCloudFormationStackDetails_outputs = Lens.lens (\AwsCloudFormationStackDetails' {outputs} -> outputs) (\s@AwsCloudFormationStackDetails' {} a -> s {outputs = a} :: AwsCloudFormationStackDetails) Prelude.. Lens.mapping Lens.coerced

-- | The capabilities allowed in the stack.
awsCloudFormationStackDetails_capabilities :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe [Prelude.Text])
awsCloudFormationStackDetails_capabilities = Lens.lens (\AwsCloudFormationStackDetails' {capabilities} -> capabilities) (\s@AwsCloudFormationStackDetails' {} a -> s {capabilities = a} :: AwsCloudFormationStackDetails) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the stack was created.
awsCloudFormationStackDetails_creationTime :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe Prelude.Text)
awsCloudFormationStackDetails_creationTime = Lens.lens (\AwsCloudFormationStackDetails' {creationTime} -> creationTime) (\s@AwsCloudFormationStackDetails' {} a -> s {creationTime = a} :: AwsCloudFormationStackDetails)

-- | Information about whether a stack\'s actual configuration differs, or
-- has drifted, from its expected configuration, as defined in the stack
-- template and any values specified as template parameters.
awsCloudFormationStackDetails_driftInformation :: Lens.Lens' AwsCloudFormationStackDetails (Prelude.Maybe AwsCloudFormationStackDriftInformationDetails)
awsCloudFormationStackDetails_driftInformation = Lens.lens (\AwsCloudFormationStackDetails' {driftInformation} -> driftInformation) (\s@AwsCloudFormationStackDetails' {} a -> s {driftInformation = a} :: AwsCloudFormationStackDetails)

instance Core.FromJSON AwsCloudFormationStackDetails where
  parseJSON =
    Core.withObject
      "AwsCloudFormationStackDetails"
      ( \x ->
          AwsCloudFormationStackDetails'
            Prelude.<$> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "TimeoutInMinutes")
            Prelude.<*> (x Core..:? "EnableTerminationProtection")
            Prelude.<*> ( x Core..:? "NotificationArns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StackStatusReason")
            Prelude.<*> (x Core..:? "DisableRollback")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "StackName")
            Prelude.<*> (x Core..:? "StackStatus")
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Capabilities" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DriftInformation")
      )

instance
  Prelude.Hashable
    AwsCloudFormationStackDetails
  where
  hashWithSalt _salt AwsCloudFormationStackDetails' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` enableTerminationProtection
      `Prelude.hashWithSalt` notificationArns
      `Prelude.hashWithSalt` stackStatusReason
      `Prelude.hashWithSalt` disableRollback
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` stackStatus
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` driftInformation

instance Prelude.NFData AwsCloudFormationStackDetails where
  rnf AwsCloudFormationStackDetails' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf enableTerminationProtection
      `Prelude.seq` Prelude.rnf notificationArns
      `Prelude.seq` Prelude.rnf stackStatusReason
      `Prelude.seq` Prelude.rnf disableRollback
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf stackStatus
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf driftInformation

instance Core.ToJSON AwsCloudFormationStackDetails where
  toJSON AwsCloudFormationStackDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StackId" Core..=) Prelude.<$> stackId,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("TimeoutInMinutes" Core..=)
              Prelude.<$> timeoutInMinutes,
            ("EnableTerminationProtection" Core..=)
              Prelude.<$> enableTerminationProtection,
            ("NotificationArns" Core..=)
              Prelude.<$> notificationArns,
            ("StackStatusReason" Core..=)
              Prelude.<$> stackStatusReason,
            ("DisableRollback" Core..=)
              Prelude.<$> disableRollback,
            ("LastUpdatedTime" Core..=)
              Prelude.<$> lastUpdatedTime,
            ("Description" Core..=) Prelude.<$> description,
            ("StackName" Core..=) Prelude.<$> stackName,
            ("StackStatus" Core..=) Prelude.<$> stackStatus,
            ("Outputs" Core..=) Prelude.<$> outputs,
            ("Capabilities" Core..=) Prelude.<$> capabilities,
            ("CreationTime" Core..=) Prelude.<$> creationTime,
            ("DriftInformation" Core..=)
              Prelude.<$> driftInformation
          ]
      )
