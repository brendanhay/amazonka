{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConfigurationItem
  ( ConfigurationItem (..)
  -- * Smart constructor
  , mkConfigurationItem
  -- * Lenses
  , ciAccountId
  , ciArn
  , ciAvailabilityZone
  , ciAwsRegion
  , ciConfiguration
  , ciConfigurationItemCaptureTime
  , ciConfigurationItemMD5Hash
  , ciConfigurationItemStatus
  , ciConfigurationStateId
  , ciRelatedEvents
  , ciRelationships
  , ciResourceCreationTime
  , ciResourceId
  , ciResourceName
  , ciResourceType
  , ciSupplementaryConfiguration
  , ciTags
  , ciVersion
  ) where

import qualified Network.AWS.Config.Types.ARN as Types
import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AvailabilityZone as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.Configuration as Types
import qualified Network.AWS.Config.Types.ConfigurationItemMD5Hash as Types
import qualified Network.AWS.Config.Types.ConfigurationItemStatus as Types
import qualified Network.AWS.Config.Types.ConfigurationStateId as Types
import qualified Network.AWS.Config.Types.Name as Types
import qualified Network.AWS.Config.Types.RelatedEvent as Types
import qualified Network.AWS.Config.Types.Relationship as Types
import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Config.Types.SupplementaryConfigurationName as Types
import qualified Network.AWS.Config.Types.SupplementaryConfigurationValue as Types
import qualified Network.AWS.Config.Types.Value as Types
import qualified Network.AWS.Config.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list that contains detailed configurations of a specified resource.
--
-- /See:/ 'mkConfigurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit AWS account ID associated with the resource.
  , arn :: Core.Maybe Types.ARN
    -- ^ accoun
  , availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone associated with the resource.
  , awsRegion :: Core.Maybe Types.AwsRegion
    -- ^ The region where the resource resides.
  , configuration :: Core.Maybe Types.Configuration
    -- ^ The description of the resource configuration.
  , configurationItemCaptureTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the configuration recording was initiated.
  , configurationItemMD5Hash :: Core.Maybe Types.ConfigurationItemMD5Hash
    -- ^ Unique MD5 hash that represents the configuration item's state.
--
-- You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
  , configurationItemStatus :: Core.Maybe Types.ConfigurationItemStatus
    -- ^ The configuration item status. The valid values are:
--
--
--     * OK – The resource configuration has been updated
--
--
--     * ResourceDiscovered – The resource was newly discovered
--
--
--     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--     * ResourceDeleted – The resource was deleted
--
--
--     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
  , configurationStateId :: Core.Maybe Types.ConfigurationStateId
    -- ^ An identifier that indicates the ordering of the configuration items of a resource.
  , relatedEvents :: Core.Maybe [Types.RelatedEvent]
    -- ^ A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> .
-- An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
  , relationships :: Core.Maybe [Types.Relationship]
    -- ^ A list of related AWS resources.
  , resourceCreationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the resource was created.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the resource (for example, @sg-xxxxxx@ ).
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ The custom name of the resource, if available.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of AWS resource.
  , supplementaryConfiguration :: Core.Maybe (Core.HashMap Types.SupplementaryConfigurationName Types.SupplementaryConfigurationValue)
    -- ^ Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
  , tags :: Core.Maybe (Core.HashMap Types.Name Types.Value)
    -- ^ A mapping of key value tags associated with the resource.
  , version :: Core.Maybe Types.Version
    -- ^ The version number of the resource configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConfigurationItem' value with any optional fields omitted.
mkConfigurationItem
    :: ConfigurationItem
mkConfigurationItem
  = ConfigurationItem'{accountId = Core.Nothing, arn = Core.Nothing,
                       availabilityZone = Core.Nothing, awsRegion = Core.Nothing,
                       configuration = Core.Nothing,
                       configurationItemCaptureTime = Core.Nothing,
                       configurationItemMD5Hash = Core.Nothing,
                       configurationItemStatus = Core.Nothing,
                       configurationStateId = Core.Nothing, relatedEvents = Core.Nothing,
                       relationships = Core.Nothing, resourceCreationTime = Core.Nothing,
                       resourceId = Core.Nothing, resourceName = Core.Nothing,
                       resourceType = Core.Nothing,
                       supplementaryConfiguration = Core.Nothing, tags = Core.Nothing,
                       version = Core.Nothing}

-- | The 12-digit AWS account ID associated with the resource.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAccountId :: Lens.Lens' ConfigurationItem (Core.Maybe Types.AccountId)
ciAccountId = Lens.field @"accountId"
{-# INLINEABLE ciAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | accoun
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciArn :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ARN)
ciArn = Lens.field @"arn"
{-# INLINEABLE ciArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The Availability Zone associated with the resource.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' ConfigurationItem (Core.Maybe Types.AvailabilityZone)
ciAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ciAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The region where the resource resides.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAwsRegion :: Lens.Lens' ConfigurationItem (Core.Maybe Types.AwsRegion)
ciAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE ciAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | The description of the resource configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfiguration :: Lens.Lens' ConfigurationItem (Core.Maybe Types.Configuration)
ciConfiguration = Lens.field @"configuration"
{-# INLINEABLE ciConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The time when the configuration recording was initiated.
--
-- /Note:/ Consider using 'configurationItemCaptureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationItemCaptureTime :: Lens.Lens' ConfigurationItem (Core.Maybe Core.NominalDiffTime)
ciConfigurationItemCaptureTime = Lens.field @"configurationItemCaptureTime"
{-# INLINEABLE ciConfigurationItemCaptureTime #-}
{-# DEPRECATED configurationItemCaptureTime "Use generic-lens or generic-optics with 'configurationItemCaptureTime' instead"  #-}

-- | Unique MD5 hash that represents the configuration item's state.
--
-- You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
--
-- /Note:/ Consider using 'configurationItemMD5Hash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationItemMD5Hash :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ConfigurationItemMD5Hash)
ciConfigurationItemMD5Hash = Lens.field @"configurationItemMD5Hash"
{-# INLINEABLE ciConfigurationItemMD5Hash #-}
{-# DEPRECATED configurationItemMD5Hash "Use generic-lens or generic-optics with 'configurationItemMD5Hash' instead"  #-}

-- | The configuration item status. The valid values are:
--
--
--     * OK – The resource configuration has been updated
--
--
--     * ResourceDiscovered – The resource was newly discovered
--
--
--     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--     * ResourceDeleted – The resource was deleted
--
--
--     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
--
--
-- /Note:/ Consider using 'configurationItemStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationItemStatus :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ConfigurationItemStatus)
ciConfigurationItemStatus = Lens.field @"configurationItemStatus"
{-# INLINEABLE ciConfigurationItemStatus #-}
{-# DEPRECATED configurationItemStatus "Use generic-lens or generic-optics with 'configurationItemStatus' instead"  #-}

-- | An identifier that indicates the ordering of the configuration items of a resource.
--
-- /Note:/ Consider using 'configurationStateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciConfigurationStateId :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ConfigurationStateId)
ciConfigurationStateId = Lens.field @"configurationStateId"
{-# INLINEABLE ciConfigurationStateId #-}
{-# DEPRECATED configurationStateId "Use generic-lens or generic-optics with 'configurationStateId' instead"  #-}

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> .
-- An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
--
-- /Note:/ Consider using 'relatedEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRelatedEvents :: Lens.Lens' ConfigurationItem (Core.Maybe [Types.RelatedEvent])
ciRelatedEvents = Lens.field @"relatedEvents"
{-# INLINEABLE ciRelatedEvents #-}
{-# DEPRECATED relatedEvents "Use generic-lens or generic-optics with 'relatedEvents' instead"  #-}

-- | A list of related AWS resources.
--
-- /Note:/ Consider using 'relationships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRelationships :: Lens.Lens' ConfigurationItem (Core.Maybe [Types.Relationship])
ciRelationships = Lens.field @"relationships"
{-# INLINEABLE ciRelationships #-}
{-# DEPRECATED relationships "Use generic-lens or generic-optics with 'relationships' instead"  #-}

-- | The time stamp when the resource was created.
--
-- /Note:/ Consider using 'resourceCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceCreationTime :: Lens.Lens' ConfigurationItem (Core.Maybe Core.NominalDiffTime)
ciResourceCreationTime = Lens.field @"resourceCreationTime"
{-# INLINEABLE ciResourceCreationTime #-}
{-# DEPRECATED resourceCreationTime "Use generic-lens or generic-optics with 'resourceCreationTime' instead"  #-}

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceId :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ResourceId)
ciResourceId = Lens.field @"resourceId"
{-# INLINEABLE ciResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The custom name of the resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceName :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ResourceName)
ciResourceName = Lens.field @"resourceName"
{-# INLINEABLE ciResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceType :: Lens.Lens' ConfigurationItem (Core.Maybe Types.ResourceType)
ciResourceType = Lens.field @"resourceType"
{-# INLINEABLE ciResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
--
-- /Note:/ Consider using 'supplementaryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSupplementaryConfiguration :: Lens.Lens' ConfigurationItem (Core.Maybe (Core.HashMap Types.SupplementaryConfigurationName Types.SupplementaryConfigurationValue))
ciSupplementaryConfiguration = Lens.field @"supplementaryConfiguration"
{-# INLINEABLE ciSupplementaryConfiguration #-}
{-# DEPRECATED supplementaryConfiguration "Use generic-lens or generic-optics with 'supplementaryConfiguration' instead"  #-}

-- | A mapping of key value tags associated with the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' ConfigurationItem (Core.Maybe (Core.HashMap Types.Name Types.Value))
ciTags = Lens.field @"tags"
{-# INLINEABLE ciTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The version number of the resource configuration.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersion :: Lens.Lens' ConfigurationItem (Core.Maybe Types.Version)
ciVersion = Lens.field @"version"
{-# INLINEABLE ciVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ConfigurationItem where
        parseJSON
          = Core.withObject "ConfigurationItem" Core.$
              \ x ->
                ConfigurationItem' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "availabilityZone"
                    Core.<*> x Core..:? "awsRegion"
                    Core.<*> x Core..:? "configuration"
                    Core.<*> x Core..:? "configurationItemCaptureTime"
                    Core.<*> x Core..:? "configurationItemMD5Hash"
                    Core.<*> x Core..:? "configurationItemStatus"
                    Core.<*> x Core..:? "configurationStateId"
                    Core.<*> x Core..:? "relatedEvents"
                    Core.<*> x Core..:? "relationships"
                    Core.<*> x Core..:? "resourceCreationTime"
                    Core.<*> x Core..:? "resourceId"
                    Core.<*> x Core..:? "resourceName"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "supplementaryConfiguration"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "version"
