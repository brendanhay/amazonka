{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformation
  ( InstanceInformation (..),

    -- * Smart constructor
    mkInstanceInformation,

    -- * Lenses
    iiInstanceId,
    iiPingStatus,
    iiIPAddress,
    iiResourceType,
    iiRegistrationDate,
    iiPlatformVersion,
    iiIsLatestVersion,
    iiAgentVersion,
    iiLastPingDateTime,
    iiLastSuccessfulAssociationExecutionDate,
    iiActivationId,
    iiName,
    iiPlatformType,
    iiAssociationOverview,
    iiAssociationStatus,
    iiLastAssociationExecutionDate,
    iiPlatformName,
    iiComputerName,
    iiIAMRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.PingStatus
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.ResourceType

-- | Describes a filter for a specific list of instances.
--
-- /See:/ 'mkInstanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { instanceId ::
      Lude.Maybe Lude.Text,
    pingStatus :: Lude.Maybe PingStatus,
    ipAddress :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    registrationDate :: Lude.Maybe Lude.Timestamp,
    platformVersion :: Lude.Maybe Lude.Text,
    isLatestVersion :: Lude.Maybe Lude.Bool,
    agentVersion :: Lude.Maybe Lude.Text,
    lastPingDateTime :: Lude.Maybe Lude.Timestamp,
    lastSuccessfulAssociationExecutionDate ::
      Lude.Maybe Lude.Timestamp,
    activationId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    platformType :: Lude.Maybe PlatformType,
    associationOverview ::
      Lude.Maybe InstanceAggregatedAssociationOverview,
    associationStatus :: Lude.Maybe Lude.Text,
    lastAssociationExecutionDate ::
      Lude.Maybe Lude.Timestamp,
    platformName :: Lude.Maybe Lude.Text,
    computerName :: Lude.Maybe Lude.Text,
    iamRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceInformation' with the minimum fields required to make a request.
--
-- * 'activationId' - The activation ID created by Systems Manager when the server or VM was registered.
-- * 'agentVersion' - The version of SSM Agent running on your Linux instance.
-- * 'associationOverview' - Information about the association.
-- * 'associationStatus' - The status of the association.
-- * 'computerName' - The fully qualified host name of the managed instance.
-- * 'iamRole' - The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
-- * 'instanceId' - The instance ID.
-- * 'ipAddress' - The IP address of the managed instance.
-- * 'isLatestVersion' - Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
-- * 'lastAssociationExecutionDate' - The date the association was last run.
-- * 'lastPingDateTime' - The date and time when agent last pinged Systems Manager service.
-- * 'lastSuccessfulAssociationExecutionDate' - The last date the association was successfully run.
-- * 'name' - The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
-- * 'pingStatus' - Connection status of SSM Agent.
-- * 'platformName' - The name of the operating system platform running on your instance.
-- * 'platformType' - The operating system platform type.
-- * 'platformVersion' - The version of the OS platform running on your instance.
-- * 'registrationDate' - The date the server or VM was registered with AWS as a managed instance.
-- * 'resourceType' - The type of instance. Instances are either EC2 instances or managed instances.
mkInstanceInformation ::
  InstanceInformation
mkInstanceInformation =
  InstanceInformation'
    { instanceId = Lude.Nothing,
      pingStatus = Lude.Nothing,
      ipAddress = Lude.Nothing,
      resourceType = Lude.Nothing,
      registrationDate = Lude.Nothing,
      platformVersion = Lude.Nothing,
      isLatestVersion = Lude.Nothing,
      agentVersion = Lude.Nothing,
      lastPingDateTime = Lude.Nothing,
      lastSuccessfulAssociationExecutionDate = Lude.Nothing,
      activationId = Lude.Nothing,
      name = Lude.Nothing,
      platformType = Lude.Nothing,
      associationOverview = Lude.Nothing,
      associationStatus = Lude.Nothing,
      lastAssociationExecutionDate = Lude.Nothing,
      platformName = Lude.Nothing,
      computerName = Lude.Nothing,
      iamRole = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceId :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiInstanceId = Lens.lens (instanceId :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceInformation)
{-# DEPRECATED iiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Connection status of SSM Agent.
--
-- /Note:/ Consider using 'pingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPingStatus :: Lens.Lens' InstanceInformation (Lude.Maybe PingStatus)
iiPingStatus = Lens.lens (pingStatus :: InstanceInformation -> Lude.Maybe PingStatus) (\s a -> s {pingStatus = a} :: InstanceInformation)
{-# DEPRECATED iiPingStatus "Use generic-lens or generic-optics with 'pingStatus' instead." #-}

-- | The IP address of the managed instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIPAddress :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiIPAddress = Lens.lens (ipAddress :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: InstanceInformation)
{-# DEPRECATED iiIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The type of instance. Instances are either EC2 instances or managed instances.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiResourceType :: Lens.Lens' InstanceInformation (Lude.Maybe ResourceType)
iiResourceType = Lens.lens (resourceType :: InstanceInformation -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: InstanceInformation)
{-# DEPRECATED iiResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The date the server or VM was registered with AWS as a managed instance.
--
-- /Note:/ Consider using 'registrationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiRegistrationDate :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Timestamp)
iiRegistrationDate = Lens.lens (registrationDate :: InstanceInformation -> Lude.Maybe Lude.Timestamp) (\s a -> s {registrationDate = a} :: InstanceInformation)
{-# DEPRECATED iiRegistrationDate "Use generic-lens or generic-optics with 'registrationDate' instead." #-}

-- | The version of the OS platform running on your instance.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformVersion :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiPlatformVersion = Lens.lens (platformVersion :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: InstanceInformation)
{-# DEPRECATED iiPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
--
-- /Note:/ Consider using 'isLatestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIsLatestVersion :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Bool)
iiIsLatestVersion = Lens.lens (isLatestVersion :: InstanceInformation -> Lude.Maybe Lude.Bool) (\s a -> s {isLatestVersion = a} :: InstanceInformation)
{-# DEPRECATED iiIsLatestVersion "Use generic-lens or generic-optics with 'isLatestVersion' instead." #-}

-- | The version of SSM Agent running on your Linux instance.
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAgentVersion :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiAgentVersion = Lens.lens (agentVersion :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {agentVersion = a} :: InstanceInformation)
{-# DEPRECATED iiAgentVersion "Use generic-lens or generic-optics with 'agentVersion' instead." #-}

-- | The date and time when agent last pinged Systems Manager service.
--
-- /Note:/ Consider using 'lastPingDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastPingDateTime :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Timestamp)
iiLastPingDateTime = Lens.lens (lastPingDateTime :: InstanceInformation -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastPingDateTime = a} :: InstanceInformation)
{-# DEPRECATED iiLastPingDateTime "Use generic-lens or generic-optics with 'lastPingDateTime' instead." #-}

-- | The last date the association was successfully run.
--
-- /Note:/ Consider using 'lastSuccessfulAssociationExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastSuccessfulAssociationExecutionDate :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Timestamp)
iiLastSuccessfulAssociationExecutionDate = Lens.lens (lastSuccessfulAssociationExecutionDate :: InstanceInformation -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulAssociationExecutionDate = a} :: InstanceInformation)
{-# DEPRECATED iiLastSuccessfulAssociationExecutionDate "Use generic-lens or generic-optics with 'lastSuccessfulAssociationExecutionDate' instead." #-}

-- | The activation ID created by Systems Manager when the server or VM was registered.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiActivationId :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiActivationId = Lens.lens (activationId :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {activationId = a} :: InstanceInformation)
{-# DEPRECATED iiActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

-- | The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiName :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiName = Lens.lens (name :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceInformation)
{-# DEPRECATED iiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operating system platform type.
--
-- /Note:/ Consider using 'platformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformType :: Lens.Lens' InstanceInformation (Lude.Maybe PlatformType)
iiPlatformType = Lens.lens (platformType :: InstanceInformation -> Lude.Maybe PlatformType) (\s a -> s {platformType = a} :: InstanceInformation)
{-# DEPRECATED iiPlatformType "Use generic-lens or generic-optics with 'platformType' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'associationOverview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAssociationOverview :: Lens.Lens' InstanceInformation (Lude.Maybe InstanceAggregatedAssociationOverview)
iiAssociationOverview = Lens.lens (associationOverview :: InstanceInformation -> Lude.Maybe InstanceAggregatedAssociationOverview) (\s a -> s {associationOverview = a} :: InstanceInformation)
{-# DEPRECATED iiAssociationOverview "Use generic-lens or generic-optics with 'associationOverview' instead." #-}

-- | The status of the association.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAssociationStatus :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiAssociationStatus = Lens.lens (associationStatus :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {associationStatus = a} :: InstanceInformation)
{-# DEPRECATED iiAssociationStatus "Use generic-lens or generic-optics with 'associationStatus' instead." #-}

-- | The date the association was last run.
--
-- /Note:/ Consider using 'lastAssociationExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastAssociationExecutionDate :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Timestamp)
iiLastAssociationExecutionDate = Lens.lens (lastAssociationExecutionDate :: InstanceInformation -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAssociationExecutionDate = a} :: InstanceInformation)
{-# DEPRECATED iiLastAssociationExecutionDate "Use generic-lens or generic-optics with 'lastAssociationExecutionDate' instead." #-}

-- | The name of the operating system platform running on your instance.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformName :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiPlatformName = Lens.lens (platformName :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {platformName = a} :: InstanceInformation)
{-# DEPRECATED iiPlatformName "Use generic-lens or generic-optics with 'platformName' instead." #-}

-- | The fully qualified host name of the managed instance.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiComputerName :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiComputerName = Lens.lens (computerName :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {computerName = a} :: InstanceInformation)
{-# DEPRECATED iiComputerName "Use generic-lens or generic-optics with 'computerName' instead." #-}

-- | The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIAMRole :: Lens.Lens' InstanceInformation (Lude.Maybe Lude.Text)
iiIAMRole = Lens.lens (iamRole :: InstanceInformation -> Lude.Maybe Lude.Text) (\s a -> s {iamRole = a} :: InstanceInformation)
{-# DEPRECATED iiIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.FromJSON InstanceInformation where
  parseJSON =
    Lude.withObject
      "InstanceInformation"
      ( \x ->
          InstanceInformation'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "PingStatus")
            Lude.<*> (x Lude..:? "IPAddress")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "RegistrationDate")
            Lude.<*> (x Lude..:? "PlatformVersion")
            Lude.<*> (x Lude..:? "IsLatestVersion")
            Lude.<*> (x Lude..:? "AgentVersion")
            Lude.<*> (x Lude..:? "LastPingDateTime")
            Lude.<*> (x Lude..:? "LastSuccessfulAssociationExecutionDate")
            Lude.<*> (x Lude..:? "ActivationId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "PlatformType")
            Lude.<*> (x Lude..:? "AssociationOverview")
            Lude.<*> (x Lude..:? "AssociationStatus")
            Lude.<*> (x Lude..:? "LastAssociationExecutionDate")
            Lude.<*> (x Lude..:? "PlatformName")
            Lude.<*> (x Lude..:? "ComputerName")
            Lude.<*> (x Lude..:? "IamRole")
      )
