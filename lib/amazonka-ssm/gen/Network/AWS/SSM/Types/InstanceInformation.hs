{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstanceInformation
  ( InstanceInformation (..)
  -- * Smart constructor
  , mkInstanceInformation
  -- * Lenses
  , iiActivationId
  , iiAgentVersion
  , iiAssociationOverview
  , iiAssociationStatus
  , iiComputerName
  , iiIPAddress
  , iiIamRole
  , iiInstanceId
  , iiIsLatestVersion
  , iiLastAssociationExecutionDate
  , iiLastPingDateTime
  , iiLastSuccessfulAssociationExecutionDate
  , iiName
  , iiPingStatus
  , iiPlatformName
  , iiPlatformType
  , iiPlatformVersion
  , iiRegistrationDate
  , iiResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ActivationId as Types
import qualified Network.AWS.SSM.Types.ComputerName as Types
import qualified Network.AWS.SSM.Types.IPAddress as Types
import qualified Network.AWS.SSM.Types.IamRole as Types
import qualified Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.PingStatus as Types
import qualified Network.AWS.SSM.Types.PlatformType as Types
import qualified Network.AWS.SSM.Types.ResourceType as Types
import qualified Network.AWS.SSM.Types.StatusName as Types
import qualified Network.AWS.SSM.Types.Version as Types

-- | Describes a filter for a specific list of instances. 
--
-- /See:/ 'mkInstanceInformation' smart constructor.
data InstanceInformation = InstanceInformation'
  { activationId :: Core.Maybe Types.ActivationId
    -- ^ The activation ID created by Systems Manager when the server or VM was registered.
  , agentVersion :: Core.Maybe Types.Version
    -- ^ The version of SSM Agent running on your Linux instance. 
  , associationOverview :: Core.Maybe Types.InstanceAggregatedAssociationOverview
    -- ^ Information about the association.
  , associationStatus :: Core.Maybe Types.StatusName
    -- ^ The status of the association.
  , computerName :: Core.Maybe Types.ComputerName
    -- ^ The fully qualified host name of the managed instance.
  , iPAddress :: Core.Maybe Types.IPAddress
    -- ^ The IP address of the managed instance.
  , iamRole :: Core.Maybe Types.IamRole
    -- ^ The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The instance ID. 
  , isLatestVersion :: Core.Maybe Core.Bool
    -- ^ Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
  , lastAssociationExecutionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the association was last run.
  , lastPingDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when agent last pinged Systems Manager service. 
  , lastSuccessfulAssociationExecutionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last date the association was successfully run.
  , name :: Core.Maybe Core.Text
    -- ^ The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
  , pingStatus :: Core.Maybe Types.PingStatus
    -- ^ Connection status of SSM Agent. 
  , platformName :: Core.Maybe Core.Text
    -- ^ The name of the operating system platform running on your instance. 
  , platformType :: Core.Maybe Types.PlatformType
    -- ^ The operating system platform type. 
  , platformVersion :: Core.Maybe Core.Text
    -- ^ The version of the OS platform running on your instance. 
  , registrationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the server or VM was registered with AWS as a managed instance.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of instance. Instances are either EC2 instances or managed instances. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceInformation' value with any optional fields omitted.
mkInstanceInformation
    :: InstanceInformation
mkInstanceInformation
  = InstanceInformation'{activationId = Core.Nothing,
                         agentVersion = Core.Nothing, associationOverview = Core.Nothing,
                         associationStatus = Core.Nothing, computerName = Core.Nothing,
                         iPAddress = Core.Nothing, iamRole = Core.Nothing,
                         instanceId = Core.Nothing, isLatestVersion = Core.Nothing,
                         lastAssociationExecutionDate = Core.Nothing,
                         lastPingDateTime = Core.Nothing,
                         lastSuccessfulAssociationExecutionDate = Core.Nothing,
                         name = Core.Nothing, pingStatus = Core.Nothing,
                         platformName = Core.Nothing, platformType = Core.Nothing,
                         platformVersion = Core.Nothing, registrationDate = Core.Nothing,
                         resourceType = Core.Nothing}

-- | The activation ID created by Systems Manager when the server or VM was registered.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiActivationId :: Lens.Lens' InstanceInformation (Core.Maybe Types.ActivationId)
iiActivationId = Lens.field @"activationId"
{-# INLINEABLE iiActivationId #-}
{-# DEPRECATED activationId "Use generic-lens or generic-optics with 'activationId' instead"  #-}

-- | The version of SSM Agent running on your Linux instance. 
--
-- /Note:/ Consider using 'agentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAgentVersion :: Lens.Lens' InstanceInformation (Core.Maybe Types.Version)
iiAgentVersion = Lens.field @"agentVersion"
{-# INLINEABLE iiAgentVersion #-}
{-# DEPRECATED agentVersion "Use generic-lens or generic-optics with 'agentVersion' instead"  #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'associationOverview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAssociationOverview :: Lens.Lens' InstanceInformation (Core.Maybe Types.InstanceAggregatedAssociationOverview)
iiAssociationOverview = Lens.field @"associationOverview"
{-# INLINEABLE iiAssociationOverview #-}
{-# DEPRECATED associationOverview "Use generic-lens or generic-optics with 'associationOverview' instead"  #-}

-- | The status of the association.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiAssociationStatus :: Lens.Lens' InstanceInformation (Core.Maybe Types.StatusName)
iiAssociationStatus = Lens.field @"associationStatus"
{-# INLINEABLE iiAssociationStatus #-}
{-# DEPRECATED associationStatus "Use generic-lens or generic-optics with 'associationStatus' instead"  #-}

-- | The fully qualified host name of the managed instance.
--
-- /Note:/ Consider using 'computerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiComputerName :: Lens.Lens' InstanceInformation (Core.Maybe Types.ComputerName)
iiComputerName = Lens.field @"computerName"
{-# INLINEABLE iiComputerName #-}
{-# DEPRECATED computerName "Use generic-lens or generic-optics with 'computerName' instead"  #-}

-- | The IP address of the managed instance.
--
-- /Note:/ Consider using 'iPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIPAddress :: Lens.Lens' InstanceInformation (Core.Maybe Types.IPAddress)
iiIPAddress = Lens.field @"iPAddress"
{-# INLINEABLE iiIPAddress #-}
{-# DEPRECATED iPAddress "Use generic-lens or generic-optics with 'iPAddress' instead"  #-}

-- | The Amazon Identity and Access Management (IAM) role assigned to the on-premises Systems Manager managed instance. This call does not return the IAM role for EC2 instances. To retrieve the IAM role for an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIamRole :: Lens.Lens' InstanceInformation (Core.Maybe Types.IamRole)
iiIamRole = Lens.field @"iamRole"
{-# INLINEABLE iiIamRole #-}
{-# DEPRECATED iamRole "Use generic-lens or generic-optics with 'iamRole' instead"  #-}

-- | The instance ID. 
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceId :: Lens.Lens' InstanceInformation (Core.Maybe Types.InstanceId)
iiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Indicates whether the latest version of SSM Agent is running on your Linux Managed Instance. This field does not indicate whether or not the latest version is installed on Windows managed instances, because some older versions of Windows Server use the EC2Config service to process SSM requests.
--
-- /Note:/ Consider using 'isLatestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIsLatestVersion :: Lens.Lens' InstanceInformation (Core.Maybe Core.Bool)
iiIsLatestVersion = Lens.field @"isLatestVersion"
{-# INLINEABLE iiIsLatestVersion #-}
{-# DEPRECATED isLatestVersion "Use generic-lens or generic-optics with 'isLatestVersion' instead"  #-}

-- | The date the association was last run.
--
-- /Note:/ Consider using 'lastAssociationExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastAssociationExecutionDate :: Lens.Lens' InstanceInformation (Core.Maybe Core.NominalDiffTime)
iiLastAssociationExecutionDate = Lens.field @"lastAssociationExecutionDate"
{-# INLINEABLE iiLastAssociationExecutionDate #-}
{-# DEPRECATED lastAssociationExecutionDate "Use generic-lens or generic-optics with 'lastAssociationExecutionDate' instead"  #-}

-- | The date and time when agent last pinged Systems Manager service. 
--
-- /Note:/ Consider using 'lastPingDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastPingDateTime :: Lens.Lens' InstanceInformation (Core.Maybe Core.NominalDiffTime)
iiLastPingDateTime = Lens.field @"lastPingDateTime"
{-# INLINEABLE iiLastPingDateTime #-}
{-# DEPRECATED lastPingDateTime "Use generic-lens or generic-optics with 'lastPingDateTime' instead"  #-}

-- | The last date the association was successfully run.
--
-- /Note:/ Consider using 'lastSuccessfulAssociationExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiLastSuccessfulAssociationExecutionDate :: Lens.Lens' InstanceInformation (Core.Maybe Core.NominalDiffTime)
iiLastSuccessfulAssociationExecutionDate = Lens.field @"lastSuccessfulAssociationExecutionDate"
{-# INLINEABLE iiLastSuccessfulAssociationExecutionDate #-}
{-# DEPRECATED lastSuccessfulAssociationExecutionDate "Use generic-lens or generic-optics with 'lastSuccessfulAssociationExecutionDate' instead"  #-}

-- | The name assigned to an on-premises server or virtual machine (VM) when it is activated as a Systems Manager managed instance. The name is specified as the @DefaultInstanceName@ property using the 'CreateActivation' command. It is applied to the managed instance by specifying the Activation Code and Activation ID when you install SSM Agent on the instance, as explained in <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-linux.html Install SSM Agent for a hybrid environment (Linux)> and <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-install-managed-win.html Install SSM Agent for a hybrid environment (Windows)> . To retrieve the Name tag of an EC2 instance, use the Amazon EC2 @DescribeInstances@ action. For information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> in the /Amazon EC2 API Reference/ or <http://docs.aws.amazon.com/cli/latest/reference/ec2/describe-instances.html describe-instances> in the /AWS CLI Command Reference/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiName :: Lens.Lens' InstanceInformation (Core.Maybe Core.Text)
iiName = Lens.field @"name"
{-# INLINEABLE iiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Connection status of SSM Agent. 
--
-- /Note:/ Consider using 'pingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPingStatus :: Lens.Lens' InstanceInformation (Core.Maybe Types.PingStatus)
iiPingStatus = Lens.field @"pingStatus"
{-# INLINEABLE iiPingStatus #-}
{-# DEPRECATED pingStatus "Use generic-lens or generic-optics with 'pingStatus' instead"  #-}

-- | The name of the operating system platform running on your instance. 
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformName :: Lens.Lens' InstanceInformation (Core.Maybe Core.Text)
iiPlatformName = Lens.field @"platformName"
{-# INLINEABLE iiPlatformName #-}
{-# DEPRECATED platformName "Use generic-lens or generic-optics with 'platformName' instead"  #-}

-- | The operating system platform type. 
--
-- /Note:/ Consider using 'platformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformType :: Lens.Lens' InstanceInformation (Core.Maybe Types.PlatformType)
iiPlatformType = Lens.field @"platformType"
{-# INLINEABLE iiPlatformType #-}
{-# DEPRECATED platformType "Use generic-lens or generic-optics with 'platformType' instead"  #-}

-- | The version of the OS platform running on your instance. 
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiPlatformVersion :: Lens.Lens' InstanceInformation (Core.Maybe Core.Text)
iiPlatformVersion = Lens.field @"platformVersion"
{-# INLINEABLE iiPlatformVersion #-}
{-# DEPRECATED platformVersion "Use generic-lens or generic-optics with 'platformVersion' instead"  #-}

-- | The date the server or VM was registered with AWS as a managed instance.
--
-- /Note:/ Consider using 'registrationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiRegistrationDate :: Lens.Lens' InstanceInformation (Core.Maybe Core.NominalDiffTime)
iiRegistrationDate = Lens.field @"registrationDate"
{-# INLINEABLE iiRegistrationDate #-}
{-# DEPRECATED registrationDate "Use generic-lens or generic-optics with 'registrationDate' instead"  #-}

-- | The type of instance. Instances are either EC2 instances or managed instances. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiResourceType :: Lens.Lens' InstanceInformation (Core.Maybe Types.ResourceType)
iiResourceType = Lens.field @"resourceType"
{-# INLINEABLE iiResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON InstanceInformation where
        parseJSON
          = Core.withObject "InstanceInformation" Core.$
              \ x ->
                InstanceInformation' Core.<$>
                  (x Core..:? "ActivationId") Core.<*> x Core..:? "AgentVersion"
                    Core.<*> x Core..:? "AssociationOverview"
                    Core.<*> x Core..:? "AssociationStatus"
                    Core.<*> x Core..:? "ComputerName"
                    Core.<*> x Core..:? "IPAddress"
                    Core.<*> x Core..:? "IamRole"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "IsLatestVersion"
                    Core.<*> x Core..:? "LastAssociationExecutionDate"
                    Core.<*> x Core..:? "LastPingDateTime"
                    Core.<*> x Core..:? "LastSuccessfulAssociationExecutionDate"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PingStatus"
                    Core.<*> x Core..:? "PlatformName"
                    Core.<*> x Core..:? "PlatformType"
                    Core.<*> x Core..:? "PlatformVersion"
                    Core.<*> x Core..:? "RegistrationDate"
                    Core.<*> x Core..:? "ResourceType"
