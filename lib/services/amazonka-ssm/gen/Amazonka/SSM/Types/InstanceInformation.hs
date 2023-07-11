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
-- Module      : Amazonka.SSM.Types.InstanceInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InstanceAggregatedAssociationOverview
import Amazonka.SSM.Types.PingStatus
import Amazonka.SSM.Types.PlatformType
import Amazonka.SSM.Types.ResourceType
import Amazonka.SSM.Types.SourceType

-- | Describes a filter for a specific list of managed nodes.
--
-- /See:/ 'newInstanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { -- | The activation ID created by Amazon Web Services Systems Manager when
    -- the server or virtual machine (VM) was registered.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The version of SSM Agent running on your Linux managed node.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about the association.
    associationOverview :: Prelude.Maybe InstanceAggregatedAssociationOverview,
    -- | The status of the association.
    associationStatus :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified host name of the managed node.
    computerName :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the managed node.
    iPAddress :: Prelude.Maybe Prelude.Text,
    -- | The Identity and Access Management (IAM) role assigned to the
    -- on-premises Systems Manager managed node. This call doesn\'t return the
    -- IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
    -- retrieve the IAM role for an EC2 instance, use the Amazon EC2
    -- @DescribeInstances@ operation. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
    -- in the /Amazon Web Services CLI Command Reference/.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the latest version of SSM Agent is running on your
    -- Linux managed node. This field doesn\'t indicate whether or not the
    -- latest version is installed on Windows managed nodes, because some older
    -- versions of Windows Server use the EC2Config service to process Systems
    -- Manager requests.
    isLatestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The date the association was last run.
    lastAssociationExecutionDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the agent last pinged the Systems Manager
    -- service.
    lastPingDateTime :: Prelude.Maybe Data.POSIX,
    -- | The last date the association was successfully run.
    lastSuccessfulAssociationExecutionDate :: Prelude.Maybe Data.POSIX,
    -- | The name assigned to an on-premises server, edge device, or virtual
    -- machine (VM) when it is activated as a Systems Manager managed node. The
    -- name is specified as the @DefaultInstanceName@ property using the
    -- CreateActivation command. It is applied to the managed node by
    -- specifying the Activation Code and Activation ID when you install SSM
    -- Agent on the node, as explained in
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
    -- and
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
    -- To retrieve the @Name@ tag of an EC2 instance, use the Amazon EC2
    -- @DescribeInstances@ operation. For information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- in the /Amazon EC2 API Reference/ or
    -- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
    -- in the /Amazon Web Services CLI Command Reference/.
    name :: Prelude.Maybe Prelude.Text,
    -- | Connection status of SSM Agent.
    --
    -- The status @Inactive@ has been deprecated and is no longer in use.
    pingStatus :: Prelude.Maybe PingStatus,
    -- | The name of the operating system platform running on your managed node.
    platformName :: Prelude.Maybe Prelude.Text,
    -- | The operating system platform type.
    platformType :: Prelude.Maybe PlatformType,
    -- | The version of the OS platform running on your managed node.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the server or VM was registered with Amazon Web Services as a
    -- managed node.
    registrationDate :: Prelude.Maybe Data.POSIX,
    -- | The type of instance. Instances are either EC2 instances or managed
    -- instances.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The ID of the source resource. For IoT Greengrass devices, @SourceId@ is
    -- the Thing name.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the source resource. For IoT Greengrass devices,
    -- @SourceType@ is @AWS::IoT::Thing@.
    sourceType :: Prelude.Maybe SourceType
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
-- 'activationId', 'instanceInformation_activationId' - The activation ID created by Amazon Web Services Systems Manager when
-- the server or virtual machine (VM) was registered.
--
-- 'agentVersion', 'instanceInformation_agentVersion' - The version of SSM Agent running on your Linux managed node.
--
-- 'associationOverview', 'instanceInformation_associationOverview' - Information about the association.
--
-- 'associationStatus', 'instanceInformation_associationStatus' - The status of the association.
--
-- 'computerName', 'instanceInformation_computerName' - The fully qualified host name of the managed node.
--
-- 'iPAddress', 'instanceInformation_iPAddress' - The IP address of the managed node.
--
-- 'iamRole', 'instanceInformation_iamRole' - The Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed node. This call doesn\'t return the
-- IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
-- retrieve the IAM role for an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
--
-- 'instanceId', 'instanceInformation_instanceId' - The managed node ID.
--
-- 'isLatestVersion', 'instanceInformation_isLatestVersion' - Indicates whether the latest version of SSM Agent is running on your
-- Linux managed node. This field doesn\'t indicate whether or not the
-- latest version is installed on Windows managed nodes, because some older
-- versions of Windows Server use the EC2Config service to process Systems
-- Manager requests.
--
-- 'lastAssociationExecutionDate', 'instanceInformation_lastAssociationExecutionDate' - The date the association was last run.
--
-- 'lastPingDateTime', 'instanceInformation_lastPingDateTime' - The date and time when the agent last pinged the Systems Manager
-- service.
--
-- 'lastSuccessfulAssociationExecutionDate', 'instanceInformation_lastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
--
-- 'name', 'instanceInformation_name' - The name assigned to an on-premises server, edge device, or virtual
-- machine (VM) when it is activated as a Systems Manager managed node. The
-- name is specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed node by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the node, as explained in
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the @Name@ tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
--
-- 'pingStatus', 'instanceInformation_pingStatus' - Connection status of SSM Agent.
--
-- The status @Inactive@ has been deprecated and is no longer in use.
--
-- 'platformName', 'instanceInformation_platformName' - The name of the operating system platform running on your managed node.
--
-- 'platformType', 'instanceInformation_platformType' - The operating system platform type.
--
-- 'platformVersion', 'instanceInformation_platformVersion' - The version of the OS platform running on your managed node.
--
-- 'registrationDate', 'instanceInformation_registrationDate' - The date the server or VM was registered with Amazon Web Services as a
-- managed node.
--
-- 'resourceType', 'instanceInformation_resourceType' - The type of instance. Instances are either EC2 instances or managed
-- instances.
--
-- 'sourceId', 'instanceInformation_sourceId' - The ID of the source resource. For IoT Greengrass devices, @SourceId@ is
-- the Thing name.
--
-- 'sourceType', 'instanceInformation_sourceType' - The type of the source resource. For IoT Greengrass devices,
-- @SourceType@ is @AWS::IoT::Thing@.
newInstanceInformation ::
  InstanceInformation
newInstanceInformation =
  InstanceInformation'
    { activationId =
        Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      associationOverview = Prelude.Nothing,
      associationStatus = Prelude.Nothing,
      computerName = Prelude.Nothing,
      iPAddress = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      isLatestVersion = Prelude.Nothing,
      lastAssociationExecutionDate = Prelude.Nothing,
      lastPingDateTime = Prelude.Nothing,
      lastSuccessfulAssociationExecutionDate =
        Prelude.Nothing,
      name = Prelude.Nothing,
      pingStatus = Prelude.Nothing,
      platformName = Prelude.Nothing,
      platformType = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      registrationDate = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The activation ID created by Amazon Web Services Systems Manager when
-- the server or virtual machine (VM) was registered.
instanceInformation_activationId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_activationId = Lens.lens (\InstanceInformation' {activationId} -> activationId) (\s@InstanceInformation' {} a -> s {activationId = a} :: InstanceInformation)

-- | The version of SSM Agent running on your Linux managed node.
instanceInformation_agentVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_agentVersion = Lens.lens (\InstanceInformation' {agentVersion} -> agentVersion) (\s@InstanceInformation' {} a -> s {agentVersion = a} :: InstanceInformation)

-- | Information about the association.
instanceInformation_associationOverview :: Lens.Lens' InstanceInformation (Prelude.Maybe InstanceAggregatedAssociationOverview)
instanceInformation_associationOverview = Lens.lens (\InstanceInformation' {associationOverview} -> associationOverview) (\s@InstanceInformation' {} a -> s {associationOverview = a} :: InstanceInformation)

-- | The status of the association.
instanceInformation_associationStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_associationStatus = Lens.lens (\InstanceInformation' {associationStatus} -> associationStatus) (\s@InstanceInformation' {} a -> s {associationStatus = a} :: InstanceInformation)

-- | The fully qualified host name of the managed node.
instanceInformation_computerName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_computerName = Lens.lens (\InstanceInformation' {computerName} -> computerName) (\s@InstanceInformation' {} a -> s {computerName = a} :: InstanceInformation)

-- | The IP address of the managed node.
instanceInformation_iPAddress :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iPAddress = Lens.lens (\InstanceInformation' {iPAddress} -> iPAddress) (\s@InstanceInformation' {} a -> s {iPAddress = a} :: InstanceInformation)

-- | The Identity and Access Management (IAM) role assigned to the
-- on-premises Systems Manager managed node. This call doesn\'t return the
-- IAM role for Amazon Elastic Compute Cloud (Amazon EC2) instances. To
-- retrieve the IAM role for an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
instanceInformation_iamRole :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_iamRole = Lens.lens (\InstanceInformation' {iamRole} -> iamRole) (\s@InstanceInformation' {} a -> s {iamRole = a} :: InstanceInformation)

-- | The managed node ID.
instanceInformation_instanceId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_instanceId = Lens.lens (\InstanceInformation' {instanceId} -> instanceId) (\s@InstanceInformation' {} a -> s {instanceId = a} :: InstanceInformation)

-- | Indicates whether the latest version of SSM Agent is running on your
-- Linux managed node. This field doesn\'t indicate whether or not the
-- latest version is installed on Windows managed nodes, because some older
-- versions of Windows Server use the EC2Config service to process Systems
-- Manager requests.
instanceInformation_isLatestVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Bool)
instanceInformation_isLatestVersion = Lens.lens (\InstanceInformation' {isLatestVersion} -> isLatestVersion) (\s@InstanceInformation' {} a -> s {isLatestVersion = a} :: InstanceInformation)

-- | The date the association was last run.
instanceInformation_lastAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastAssociationExecutionDate} -> lastAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Data._Time

-- | The date and time when the agent last pinged the Systems Manager
-- service.
instanceInformation_lastPingDateTime :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastPingDateTime = Lens.lens (\InstanceInformation' {lastPingDateTime} -> lastPingDateTime) (\s@InstanceInformation' {} a -> s {lastPingDateTime = a} :: InstanceInformation) Prelude.. Lens.mapping Data._Time

-- | The last date the association was successfully run.
instanceInformation_lastSuccessfulAssociationExecutionDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_lastSuccessfulAssociationExecutionDate = Lens.lens (\InstanceInformation' {lastSuccessfulAssociationExecutionDate} -> lastSuccessfulAssociationExecutionDate) (\s@InstanceInformation' {} a -> s {lastSuccessfulAssociationExecutionDate = a} :: InstanceInformation) Prelude.. Lens.mapping Data._Time

-- | The name assigned to an on-premises server, edge device, or virtual
-- machine (VM) when it is activated as a Systems Manager managed node. The
-- name is specified as the @DefaultInstanceName@ property using the
-- CreateActivation command. It is applied to the managed node by
-- specifying the Activation Code and Activation ID when you install SSM
-- Agent on the node, as explained in
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)>
-- and
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)>.
-- To retrieve the @Name@ tag of an EC2 instance, use the Amazon EC2
-- @DescribeInstances@ operation. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- in the /Amazon EC2 API Reference/ or
-- <https://docs.aws.amazon.com/cli/latest/ec2/describe-instances.html describe-instances>
-- in the /Amazon Web Services CLI Command Reference/.
instanceInformation_name :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_name = Lens.lens (\InstanceInformation' {name} -> name) (\s@InstanceInformation' {} a -> s {name = a} :: InstanceInformation)

-- | Connection status of SSM Agent.
--
-- The status @Inactive@ has been deprecated and is no longer in use.
instanceInformation_pingStatus :: Lens.Lens' InstanceInformation (Prelude.Maybe PingStatus)
instanceInformation_pingStatus = Lens.lens (\InstanceInformation' {pingStatus} -> pingStatus) (\s@InstanceInformation' {} a -> s {pingStatus = a} :: InstanceInformation)

-- | The name of the operating system platform running on your managed node.
instanceInformation_platformName :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformName = Lens.lens (\InstanceInformation' {platformName} -> platformName) (\s@InstanceInformation' {} a -> s {platformName = a} :: InstanceInformation)

-- | The operating system platform type.
instanceInformation_platformType :: Lens.Lens' InstanceInformation (Prelude.Maybe PlatformType)
instanceInformation_platformType = Lens.lens (\InstanceInformation' {platformType} -> platformType) (\s@InstanceInformation' {} a -> s {platformType = a} :: InstanceInformation)

-- | The version of the OS platform running on your managed node.
instanceInformation_platformVersion :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_platformVersion = Lens.lens (\InstanceInformation' {platformVersion} -> platformVersion) (\s@InstanceInformation' {} a -> s {platformVersion = a} :: InstanceInformation)

-- | The date the server or VM was registered with Amazon Web Services as a
-- managed node.
instanceInformation_registrationDate :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.UTCTime)
instanceInformation_registrationDate = Lens.lens (\InstanceInformation' {registrationDate} -> registrationDate) (\s@InstanceInformation' {} a -> s {registrationDate = a} :: InstanceInformation) Prelude.. Lens.mapping Data._Time

-- | The type of instance. Instances are either EC2 instances or managed
-- instances.
instanceInformation_resourceType :: Lens.Lens' InstanceInformation (Prelude.Maybe ResourceType)
instanceInformation_resourceType = Lens.lens (\InstanceInformation' {resourceType} -> resourceType) (\s@InstanceInformation' {} a -> s {resourceType = a} :: InstanceInformation)

-- | The ID of the source resource. For IoT Greengrass devices, @SourceId@ is
-- the Thing name.
instanceInformation_sourceId :: Lens.Lens' InstanceInformation (Prelude.Maybe Prelude.Text)
instanceInformation_sourceId = Lens.lens (\InstanceInformation' {sourceId} -> sourceId) (\s@InstanceInformation' {} a -> s {sourceId = a} :: InstanceInformation)

-- | The type of the source resource. For IoT Greengrass devices,
-- @SourceType@ is @AWS::IoT::Thing@.
instanceInformation_sourceType :: Lens.Lens' InstanceInformation (Prelude.Maybe SourceType)
instanceInformation_sourceType = Lens.lens (\InstanceInformation' {sourceType} -> sourceType) (\s@InstanceInformation' {} a -> s {sourceType = a} :: InstanceInformation)

instance Data.FromJSON InstanceInformation where
  parseJSON =
    Data.withObject
      "InstanceInformation"
      ( \x ->
          InstanceInformation'
            Prelude.<$> (x Data..:? "ActivationId")
            Prelude.<*> (x Data..:? "AgentVersion")
            Prelude.<*> (x Data..:? "AssociationOverview")
            Prelude.<*> (x Data..:? "AssociationStatus")
            Prelude.<*> (x Data..:? "ComputerName")
            Prelude.<*> (x Data..:? "IPAddress")
            Prelude.<*> (x Data..:? "IamRole")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "IsLatestVersion")
            Prelude.<*> (x Data..:? "LastAssociationExecutionDate")
            Prelude.<*> (x Data..:? "LastPingDateTime")
            Prelude.<*> (x Data..:? "LastSuccessfulAssociationExecutionDate")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PingStatus")
            Prelude.<*> (x Data..:? "PlatformName")
            Prelude.<*> (x Data..:? "PlatformType")
            Prelude.<*> (x Data..:? "PlatformVersion")
            Prelude.<*> (x Data..:? "RegistrationDate")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SourceId")
            Prelude.<*> (x Data..:? "SourceType")
      )

instance Prelude.Hashable InstanceInformation where
  hashWithSalt _salt InstanceInformation' {..} =
    _salt
      `Prelude.hashWithSalt` activationId
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` associationOverview
      `Prelude.hashWithSalt` associationStatus
      `Prelude.hashWithSalt` computerName
      `Prelude.hashWithSalt` iPAddress
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` isLatestVersion
      `Prelude.hashWithSalt` lastAssociationExecutionDate
      `Prelude.hashWithSalt` lastPingDateTime
      `Prelude.hashWithSalt` lastSuccessfulAssociationExecutionDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pingStatus
      `Prelude.hashWithSalt` platformName
      `Prelude.hashWithSalt` platformType
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` registrationDate
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData InstanceInformation where
  rnf InstanceInformation' {..} =
    Prelude.rnf activationId
      `Prelude.seq` Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf associationOverview
      `Prelude.seq` Prelude.rnf associationStatus
      `Prelude.seq` Prelude.rnf computerName
      `Prelude.seq` Prelude.rnf iPAddress
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf isLatestVersion
      `Prelude.seq` Prelude.rnf lastAssociationExecutionDate
      `Prelude.seq` Prelude.rnf lastPingDateTime
      `Prelude.seq` Prelude.rnf
        lastSuccessfulAssociationExecutionDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pingStatus
      `Prelude.seq` Prelude.rnf platformName
      `Prelude.seq` Prelude.rnf platformType
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf registrationDate
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf sourceType
