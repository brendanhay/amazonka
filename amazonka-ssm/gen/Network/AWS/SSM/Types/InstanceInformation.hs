{-# LANGUAGE DeriveDataTypeable #-}
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
    -- | The activation ID created by Systems Manager when the server or VM was
    -- registered.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Identity and Access Management (IAM) role assigned to the
    -- on-premises Systems Manager managed instance. This call does not return
    -- the IAM role for EC2 instances. To retrieve the IAM role for an EC2
    -- instance, use the Amazon EC2 @DescribeInstances@ action. For
    -- information, see
    -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
    -- in the /AWS CLI Command Reference/.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The last date the association was successfully run.
    lastSuccessfulAssociationExecutionDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the agent last pinged the Systems Manager
    -- service.
    lastPingDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The version of SSM Agent running on your Linux instance.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the OS platform running on your instance.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the association was last run.
    lastAssociationExecutionDate :: Prelude.Maybe Prelude.POSIX,
    -- | The type of instance. Instances are either EC2 instances or managed
    -- instances.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Information about the association.
    associationOverview :: Prelude.Maybe InstanceAggregatedAssociationOverview,
    -- | The IP address of the managed instance.
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | The name assigned to an on-premises server or virtual machine (VM) when
    -- it is activated as a Systems Manager managed instance. The name is
    -- specified as the @DefaultInstanceName@ property using the
    -- CreateActivation command. It is applied to the managed instance by
    -- specifying the Activation Code and Activation ID when you install SSM
    -- Agent on the instance, as explained in
    -- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
    -- and
    -- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
    -- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
    -- @DescribeInstances@ action. For information, see
    -- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
    -- in the /AWS CLI Command Reference/.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system platform type.
    platformType :: Prelude.Maybe PlatformType,
    -- | Indicates whether the latest version of SSM Agent is running on your
    -- Linux Managed Instance. This field does not indicate whether or not the
    -- latest version is installed on Windows managed instances, because some
    -- older versions of Windows Server use the EC2Config service to process
    -- SSM requests.
    isLatestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The fully qualified host name of the managed instance.
    computerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the operating system platform running on your instance.
    platformName :: Prelude.Maybe Prelude.Text,
    -- | The date the server or VM was registered with AWS as a managed instance.
    registrationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the association.
    associationStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'activationId', 'instanceInformation_activationId' - The activation ID created by Systems Manager when the server or VM was
-- registered.
--
-- 'iamRole', 'instanceInformation_iamRole' - The Amazon Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed instance. This call does not return
-- the IAM role for EC2 instances. To retrieve the IAM role for an EC2
-- instance, use the Amazon EC2 @DescribeInstances@ action. For
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
-- in the /AWS CLI Command Reference/.
--
-- 'lastSuccessfulAssociationExecutionDate', 'instanceInformation_lastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- 'lastPingDateTime', 'instanceInformation_lastPingDateTime' - The date and time when the agent last pinged the Systems Manager
-- service.
--
-- 'agentVersion', 'instanceInformation_agentVersion' - The version of SSM Agent running on your Linux instance.
--
-- 'platformVersion', 'instanceInformation_platformVersion' - The version of the OS platform running on your instance.
--
-- 'lastAssociationExecutionDate', 'instanceInformation_lastAssociationExecutionDate' - The date the association was last run.
--
-- 'resourceType', 'instanceInformation_resourceType' - The type of instance. Instances are either EC2 instances or managed
-- instances.
--
-- 'associationOverview', 'instanceInformation_associationOverview' - Information about the association.
--
-- 'iPAddress', 'instanceInformation_iPAddress' - The IP address of the managed instance.
--
-- 'name', 'instanceInformation_name' - The name assigned to an on-premises server or virtual machine (VM) when
-- it is activated as a Systems Manager managed instance. The name is
-- specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed instance by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the instance, as explained in
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ action. For information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
-- in the /AWS CLI Command Reference/.
--
-- 'platformType', 'instanceInformation_platformType' - The operating system platform type.
--
-- 'isLatestVersion', 'instanceInformation_isLatestVersion' - Indicates whether the latest version of SSM Agent is running on your
-- Linux Managed Instance. This field does not indicate whether or not the
-- latest version is installed on Windows managed instances, because some
-- older versions of Windows Server use the EC2Config service to process
-- SSM requests.
--
-- 'computerName', 'instanceInformation_computerName' - The fully qualified host name of the managed instance.
--
-- 'platformName', 'instanceInformation_platformName' - The name of the operating system platform running on your instance.
--
-- 'registrationDate', 'instanceInformation_registrationDate' - The date the server or VM was registered with AWS as a managed instance.
--
-- 'associationStatus', 'instanceInformation_associationStatus' - The status of the association.
newInstanceInformation ::
  InstanceInformation
newInstanceInformation =
  InstanceInformation'
    { instanceId = Prelude.Nothing,
      pingStatus = Prelude.Nothing,
      activationId = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      lastSuccessfulAssociationExecutionDate =
        Prelude.Nothing,
      lastPingDateTime = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      lastAssociationExecutionDate = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      associationOverview = Prelude.Nothing,
      iPAddress = Prelude.Nothing,
      name = Prelude.Nothing,
      platformType = Prelude.Nothing,
      isLatestVersion = Prelude.Nothing,
      computerName = Prelude.Nothing,
      platformName = Prelude.Nothing,
      registrationDate = Prelude.Nothing,
      associationStatus = Prelude.Nothing
    }

-- | The instance ID.
instanceInformation_instanceId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_instanceId = Lens.lens (\InstanceInformation' {instanceId} -> instanceId) (\s@InstanceInformation' {} a -> s {instanceId = a} :: InstanceInformation)

-- | Connection status of SSM Agent.
--
-- The status @Inactive@ has been deprecated and is no longer in use.
instanceInformation_pingStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe PingStatus)
instanceInformation_pingStatus = Lens.lens (\InstanceInformation' {pingStatus} -> pingStatus) (\s@InstanceInformation' {} a -> s {pingStatus = a} :: InstanceInformation)

-- | The activation ID created by Systems Manager when the server or VM was
-- registered.
instanceInformation_activationId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_activationId = Lens.lens (\InstanceInformation' {activationId} -> activationId) (\s@InstanceInformation' {} a -> s {activationId = a} :: InstanceInformation)

-- | The Amazon Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed instance. This call does not return
-- the IAM role for EC2 instances. To retrieve the IAM role for an EC2
-- instance, use the Amazon EC2 @DescribeInstances@ action. For
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
-- in the /AWS CLI Command Reference/.
instanceInformation_iamRole :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iamRole = Lens.lens (\InstanceInformation' {iamRole} -> iamRole) (\s@InstanceInformation' {} a -> s {iamRole = a} :: InstanceInformation)

-- | The last date the association was successfully run.
instanceInformation_lastSuccessfulAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastSuccessfulAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastSuccessfulAssociationExecutionDate} -> lastSuccessfulAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastSuccessfulAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the agent last pinged the Systems Manager
-- service.
instanceInformation_lastPingDateTime :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastPingDateTime = Lens.lens (\InstanceInformation' {lastPingDateTime} -> lastPingDateTime) (\s@InstanceInformation' {} a -> s {lastPingDateTime = a} :: InstanceInformation) Prelude.. Lens.mapping Prelude._Time

-- | The version of SSM Agent running on your Linux instance.
instanceInformation_agentVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_agentVersion = Lens.lens (\InstanceInformation' {agentVersion} -> agentVersion) (\s@InstanceInformation' {} a -> s {agentVersion = a} :: InstanceInformation)

-- | The version of the OS platform running on your instance.
instanceInformation_platformVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformVersion = Lens.lens (\InstanceInformation' {platformVersion} -> platformVersion) (\s@InstanceInformation' {} a -> s {platformVersion = a} :: InstanceInformation)

-- | The date the association was last run.
instanceInformation_lastAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastAssociationExecutionDate} -> lastAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Prelude._Time

-- | The type of instance. Instances are either EC2 instances or managed
-- instances.
instanceInformation_resourceType :: Lens.Lens' InstanceInformation (Prelude.Maybe ResourceType)
instanceInformation_resourceType = Lens.lens (\InstanceInformation' {resourceType} -> resourceType) (\s@InstanceInformation' {} a -> s {resourceType = a} :: InstanceInformation)

-- | Information about the association.
instanceInformation_associationOverview :: Lens.Lens' InstanceInformation (Prelude.Maybe InstanceAggregatedAssociationOverview)
instanceInformation_associationOverview = Lens.lens (\InstanceInformation' {associationOverview} -> associationOverview) (\s@InstanceInformation' {} a -> s {associationOverview = a} :: InstanceInformation)

-- | The IP address of the managed instance.
instanceInformation_iPAddress :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iPAddress = Lens.lens (\InstanceInformation' {iPAddress} -> iPAddress) (\s@InstanceInformation' {} a -> s {iPAddress = a} :: InstanceInformation)

-- | The name assigned to an on-premises server or virtual machine (VM) when
-- it is activated as a Systems Manager managed instance. The name is
-- specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed instance by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the instance, as explained in
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the Name tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ action. For information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances>
-- in the /AWS CLI Command Reference/.
instanceInformation_name :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_name = Lens.lens (\InstanceInformation' {name} -> name) (\s@InstanceInformation' {} a -> s {name = a} :: InstanceInformation)

-- | The operating system platform type.
instanceInformation_platformType :: Lens.Lens' InstanceInformation (Prelude.Maybe PlatformType)
instanceInformation_platformType = Lens.lens (\InstanceInformation' {platformType} -> platformType) (\s@InstanceInformation' {} a -> s {platformType = a} :: InstanceInformation)

-- | Indicates whether the latest version of SSM Agent is running on your
-- Linux Managed Instance. This field does not indicate whether or not the
-- latest version is installed on Windows managed instances, because some
-- older versions of Windows Server use the EC2Config service to process
-- SSM requests.
instanceInformation_isLatestVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Bool)
instanceInformation_isLatestVersion = Lens.lens (\InstanceInformation' {isLatestVersion} -> isLatestVersion) (\s@InstanceInformation' {} a -> s {isLatestVersion = a} :: InstanceInformation)

-- | The fully qualified host name of the managed instance.
instanceInformation_computerName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_computerName = Lens.lens (\InstanceInformation' {computerName} -> computerName) (\s@InstanceInformation' {} a -> s {computerName = a} :: InstanceInformation)

-- | The name of the operating system platform running on your instance.
instanceInformation_platformName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformName = Lens.lens (\InstanceInformation' {platformName} -> platformName) (\s@InstanceInformation' {} a -> s {platformName = a} :: InstanceInformation)

-- | The date the server or VM was registered with AWS as a managed instance.
instanceInformation_registrationDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_registrationDate = Lens.lens (\InstanceInformation' {registrationDate} -> registrationDate) (\s@InstanceInformation' {} a -> s {registrationDate = a} :: InstanceInformation) Prelude.. Lens.mapping Prelude._Time

-- | The status of the association.
instanceInformation_associationStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_associationStatus = Lens.lens (\InstanceInformation' {associationStatus} -> associationStatus) (\s@InstanceInformation' {} a -> s {associationStatus = a} :: InstanceInformation)

instance Prelude.FromJSON InstanceInformation where
  parseJSON =
    Prelude.withObject
      "InstanceInformation"
      ( \x ->
          InstanceInformation'
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "PingStatus")
            Prelude.<*> (x Prelude..:? "ActivationId")
            Prelude.<*> (x Prelude..:? "IamRole")
            Prelude.<*> ( x
                            Prelude..:? "LastSuccessfulAssociationExecutionDate"
                        )
            Prelude.<*> (x Prelude..:? "LastPingDateTime")
            Prelude.<*> (x Prelude..:? "AgentVersion")
            Prelude.<*> (x Prelude..:? "PlatformVersion")
            Prelude.<*> (x Prelude..:? "LastAssociationExecutionDate")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "AssociationOverview")
            Prelude.<*> (x Prelude..:? "IPAddress")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "PlatformType")
            Prelude.<*> (x Prelude..:? "IsLatestVersion")
            Prelude.<*> (x Prelude..:? "ComputerName")
            Prelude.<*> (x Prelude..:? "PlatformName")
            Prelude.<*> (x Prelude..:? "RegistrationDate")
            Prelude.<*> (x Prelude..:? "AssociationStatus")
      )

instance Prelude.Hashable InstanceInformation

instance Prelude.NFData InstanceInformation
