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
-- Module      : Network.AWS.SSM.Types.InstanceInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.PingStatus
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ResourceType

-- | Describes a filter for a specific list of instances.
--
-- /See:/ 'newInstanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { -- | The instance ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Connection status of SSM Agent.
    --
    -- The status @Inactive@ has been deprecated and is no longer in use.
    pingStatus :: Prelude.Maybe PingStatus,
    -- | The IP address of the managed instance.
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | The type of instance. Instances are either EC2 instances or managed
    -- instances.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The date the server or VM was registered with Amazon Web Services as a
    -- managed instance.
    registrationDate :: Prelude.Maybe Core.POSIX,
    -- | The version of the OS platform running on your instance.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the latest version of SSM Agent is running on your
    -- Linux Managed Instance. This field doesn\'t indicate whether or not the
    -- latest version is installed on Windows managed instances, because some
    -- older versions of Windows Server use the EC2Config service to process
    -- Systems Manager requests.
    isLatestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The version of SSM Agent running on your Linux instance.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the agent last pinged the Systems Manager
    -- service.
    lastPingDateTime :: Prelude.Maybe Core.POSIX,
    -- | The last date the association was successfully run.
    lastSuccessfulAssociationExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The activation ID created by Amazon Web Services Systems Manager when
    -- the server or virtual machine (VM) was registered.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The name assigned to an on-premises server or virtual machine (VM) when
    -- it is activated as a Systems Manager managed instance. The name is
    -- specified as the @DefaultInstanceName@ property using the
    -- CreateActivation command. It is applied to the managed instance by
    -- specifying the Activation Code and Activation ID when you install SSM
    -- Agent on the instance, as explained in
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
    -- and
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
    -- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
    -- @DescribeInstances@ operation. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
    -- in the /Amazon Web Services CLI Command Reference/.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system platform type.
    platformType :: Prelude.Maybe PlatformType,
    -- | Information about the association.
    associationOverview :: Prelude.Maybe InstanceAggregatedAssociationOverview,
    -- | The status of the association.
    associationStatus :: Prelude.Maybe Prelude.Text,
    -- | The date the association was last run.
    lastAssociationExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the operating system platform running on your instance.
    platformName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified host name of the managed instance.
    computerName :: Prelude.Maybe Prelude.Text,
    -- | The Identity and Access Management (IAM) role assigned to the
    -- on-premises Systems Manager managed instance. This call doesn\'t return
    -- the IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
    -- retrieve the IAM role for an EC2 instance, use the Amazon EC2
    -- @DescribeInstances@ operation. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
    -- in the /Amazon Web Services CLI Command Reference/.
    iamRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceInformation_instanceId' - The instance ID.
--
-- 'pingStatus', 'instanceInformation_pingStatus' - Connection status of SSM Agent.
--
-- The status @Inactive@ has been deprecated and is no longer in use.
--
-- 'iPAddress', 'instanceInformation_iPAddress' - The IP address of the managed instance.
--
-- 'resourceType', 'instanceInformation_resourceType' - The type of instance. Instances are either EC2 instances or managed
-- instances.
--
-- 'registrationDate', 'instanceInformation_registrationDate' - The date the server or VM was registered with Amazon Web Services as a
-- managed instance.
--
-- 'platformVersion', 'instanceInformation_platformVersion' - The version of the OS platform running on your instance.
--
-- 'isLatestVersion', 'instanceInformation_isLatestVersion' - Indicates whether the latest version of SSM Agent is running on your
-- Linux Managed Instance. This field doesn\'t indicate whether or not the
-- latest version is installed on Windows managed instances, because some
-- older versions of Windows Server use the EC2Config service to process
-- Systems Manager requests.
--
-- 'agentVersion', 'instanceInformation_agentVersion' - The version of SSM Agent running on your Linux instance.
--
-- 'lastPingDateTime', 'instanceInformation_lastPingDateTime' - The date and time when the agent last pinged the Systems Manager
-- service.
--
-- 'lastSuccessfulAssociationExecutionDate', 'instanceInformation_lastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- 'activationId', 'instanceInformation_activationId' - The activation ID created by Amazon Web Services Systems Manager when
-- the server or virtual machine (VM) was registered.
--
-- 'name', 'instanceInformation_name' - The name assigned to an on-premises server or virtual machine (VM) when
-- it is activated as a Systems Manager managed instance. The name is
-- specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed instance by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the instance, as explained in
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
--
-- 'platformType', 'instanceInformation_platformType' - The operating system platform type.
--
-- 'associationOverview', 'instanceInformation_associationOverview' - Information about the association.
--
-- 'associationStatus', 'instanceInformation_associationStatus' - The status of the association.
--
-- 'lastAssociationExecutionDate', 'instanceInformation_lastAssociationExecutionDate' - The date the association was last run.
--
-- 'platformName', 'instanceInformation_platformName' - The name of the operating system platform running on your instance.
--
-- 'computerName', 'instanceInformation_computerName' - The fully qualified host name of the managed instance.
--
-- 'iamRole', 'instanceInformation_iamRole' - The Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed instance. This call doesn\'t return
-- the IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
-- retrieve the IAM role for an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
newInstanceInformation ::
  InstanceInformation
newInstanceInformation =
  InstanceInformation'
    { instanceId = Prelude.Nothing,
      pingStatus = Prelude.Nothing,
      iPAddress = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      registrationDate = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      isLatestVersion = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      lastPingDateTime = Prelude.Nothing,
      lastSuccessfulAssociationExecutionDate =
        Prelude.Nothing,
      activationId = Prelude.Nothing,
      name = Prelude.Nothing,
      platformType = Prelude.Nothing,
      associationOverview = Prelude.Nothing,
      associationStatus = Prelude.Nothing,
      lastAssociationExecutionDate = Prelude.Nothing,
      platformName = Prelude.Nothing,
      computerName = Prelude.Nothing,
      iamRole = Prelude.Nothing
    }

-- | The instance ID.
instanceInformation_instanceId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_instanceId = Lens.lens (\InstanceInformation' {instanceId} -> instanceId) (\s@InstanceInformation' {} a -> s {instanceId = a} :: InstanceInformation)

-- | Connection status of SSM Agent.
--
-- The status @Inactive@ has been deprecated and is no longer in use.
instanceInformation_pingStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe PingStatus)
instanceInformation_pingStatus = Lens.lens (\InstanceInformation' {pingStatus} -> pingStatus) (\s@InstanceInformation' {} a -> s {pingStatus = a} :: InstanceInformation)

-- | The IP address of the managed instance.
instanceInformation_iPAddress :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iPAddress = Lens.lens (\InstanceInformation' {iPAddress} -> iPAddress) (\s@InstanceInformation' {} a -> s {iPAddress = a} :: InstanceInformation)

-- | The type of instance. Instances are either EC2 instances or managed
-- instances.
instanceInformation_resourceType :: Lens.Lens' InstanceInformation (Prelude.Maybe ResourceType)
instanceInformation_resourceType = Lens.lens (\InstanceInformation' {resourceType} -> resourceType) (\s@InstanceInformation' {} a -> s {resourceType = a} :: InstanceInformation)

-- | The date the server or VM was registered with Amazon Web Services as a
-- managed instance.
instanceInformation_registrationDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_registrationDate = Lens.lens (\InstanceInformation' {registrationDate} -> registrationDate) (\s@InstanceInformation' {} a -> s {registrationDate = a} :: InstanceInformation) Prelude.. Lens.mapping Core._Time

-- | The version of the OS platform running on your instance.
instanceInformation_platformVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformVersion = Lens.lens (\InstanceInformation' {platformVersion} -> platformVersion) (\s@InstanceInformation' {} a -> s {platformVersion = a} :: InstanceInformation)

-- | Indicates whether the latest version of SSM Agent is running on your
-- Linux Managed Instance. This field doesn\'t indicate whether or not the
-- latest version is installed on Windows managed instances, because some
-- older versions of Windows Server use the EC2Config service to process
-- Systems Manager requests.
instanceInformation_isLatestVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Bool)
instanceInformation_isLatestVersion = Lens.lens (\InstanceInformation' {isLatestVersion} -> isLatestVersion) (\s@InstanceInformation' {} a -> s {isLatestVersion = a} :: InstanceInformation)

-- | The version of SSM Agent running on your Linux instance.
instanceInformation_agentVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_agentVersion = Lens.lens (\InstanceInformation' {agentVersion} -> agentVersion) (\s@InstanceInformation' {} a -> s {agentVersion = a} :: InstanceInformation)

-- | The date and time when the agent last pinged the Systems Manager
-- service.
instanceInformation_lastPingDateTime :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastPingDateTime = Lens.lens (\InstanceInformation' {lastPingDateTime} -> lastPingDateTime) (\s@InstanceInformation' {} a -> s {lastPingDateTime = a} :: InstanceInformation) Prelude.. Lens.mapping Core._Time

-- | The last date the association was successfully run.
instanceInformation_lastSuccessfulAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastSuccessfulAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastSuccessfulAssociationExecutionDate} -> lastSuccessfulAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastSuccessfulAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Core._Time

-- | The activation ID created by Amazon Web Services Systems Manager when
-- the server or virtual machine (VM) was registered.
instanceInformation_activationId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_activationId = Lens.lens (\InstanceInformation' {activationId} -> activationId) (\s@InstanceInformation' {} a -> s {activationId = a} :: InstanceInformation)

-- | The name assigned to an on-premises server or virtual machine (VM) when
-- it is activated as a Systems Manager managed instance. The name is
-- specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed instance by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the instance, as explained in
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
instanceInformation_name :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_name = Lens.lens (\InstanceInformation' {name} -> name) (\s@InstanceInformation' {} a -> s {name = a} :: InstanceInformation)

-- | The operating system platform type.
instanceInformation_platformType :: Lens.Lens' InstanceInformation (Prelude.Maybe PlatformType)
instanceInformation_platformType = Lens.lens (\InstanceInformation' {platformType} -> platformType) (\s@InstanceInformation' {} a -> s {platformType = a} :: InstanceInformation)

-- | Information about the association.
instanceInformation_associationOverview :: Lens.Lens' InstanceInformation (Prelude.Maybe InstanceAggregatedAssociationOverview)
instanceInformation_associationOverview = Lens.lens (\InstanceInformation' {associationOverview} -> associationOverview) (\s@InstanceInformation' {} a -> s {associationOverview = a} :: InstanceInformation)

-- | The status of the association.
instanceInformation_associationStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_associationStatus = Lens.lens (\InstanceInformation' {associationStatus} -> associationStatus) (\s@InstanceInformation' {} a -> s {associationStatus = a} :: InstanceInformation)

-- | The date the association was last run.
instanceInformation_lastAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastAssociationExecutionDate} -> lastAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Core._Time

-- | The name of the operating system platform running on your instance.
instanceInformation_platformName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformName = Lens.lens (\InstanceInformation' {platformName} -> platformName) (\s@InstanceInformation' {} a -> s {platformName = a} :: InstanceInformation)

-- | The fully qualified host name of the managed instance.
instanceInformation_computerName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_computerName = Lens.lens (\InstanceInformation' {computerName} -> computerName) (\s@InstanceInformation' {} a -> s {computerName = a} :: InstanceInformation)

-- | The Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed instance. This call doesn\'t return
-- the IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
-- retrieve the IAM role for an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
instanceInformation_iamRole :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iamRole = Lens.lens (\InstanceInformation' {iamRole} -> iamRole) (\s@InstanceInformation' {} a -> s {iamRole = a} :: InstanceInformation)

instance Core.FromJSON InstanceInformation where
  parseJSON =
    Core.withObject
      "InstanceInformation"
      ( \x ->
          InstanceInformation'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "PingStatus")
            Prelude.<*> (x Core..:? "IPAddress")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "RegistrationDate")
            Prelude.<*> (x Core..:? "PlatformVersion")
            Prelude.<*> (x Core..:? "IsLatestVersion")
            Prelude.<*> (x Core..:? "AgentVersion")
            Prelude.<*> (x Core..:? "LastPingDateTime")
            Prelude.<*> (x Core..:? "LastSuccessfulAssociationExecutionDate")
            Prelude.<*> (x Core..:? "ActivationId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "PlatformType")
            Prelude.<*> (x Core..:? "AssociationOverview")
            Prelude.<*> (x Core..:? "AssociationStatus")
            Prelude.<*> (x Core..:? "LastAssociationExecutionDate")
            Prelude.<*> (x Core..:? "PlatformName")
            Prelude.<*> (x Core..:? "ComputerName")
            Prelude.<*> (x Core..:? "IamRole")
      )

instance Prelude.Hashable InstanceInformation

instance Prelude.NFData InstanceInformation
