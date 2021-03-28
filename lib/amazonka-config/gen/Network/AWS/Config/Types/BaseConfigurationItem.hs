{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.BaseConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.BaseConfigurationItem
  ( BaseConfigurationItem (..)
  -- * Smart constructor
  , mkBaseConfigurationItem
  -- * Lenses
  , bciAccountId
  , bciArn
  , bciAvailabilityZone
  , bciAwsRegion
  , bciConfiguration
  , bciConfigurationItemCaptureTime
  , bciConfigurationItemStatus
  , bciConfigurationStateId
  , bciResourceCreationTime
  , bciResourceId
  , bciResourceName
  , bciResourceType
  , bciSupplementaryConfiguration
  , bciVersion
  ) where

import qualified Network.AWS.Config.Types.ARN as Types
import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AvailabilityZone as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.Configuration as Types
import qualified Network.AWS.Config.Types.ConfigurationItemStatus as Types
import qualified Network.AWS.Config.Types.ConfigurationStateId as Types
import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Config.Types.SupplementaryConfigurationName as Types
import qualified Network.AWS.Config.Types.SupplementaryConfigurationValue as Types
import qualified Network.AWS.Config.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The detailed configuration of a specified resource.
--
-- /See:/ 'mkBaseConfigurationItem' smart constructor.
data BaseConfigurationItem = BaseConfigurationItem'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit AWS account ID associated with the resource.
  , arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the resource.
  , availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone associated with the resource.
  , awsRegion :: Core.Maybe Types.AwsRegion
    -- ^ The region where the resource resides.
  , configuration :: Core.Maybe Types.Configuration
    -- ^ The description of the resource configuration.
  , configurationItemCaptureTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the configuration recording was initiated.
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
  , resourceCreationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the resource was created.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the resource (for example., sg-xxxxxx).
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ The custom name of the resource, if available.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of AWS resource.
  , supplementaryConfiguration :: Core.Maybe (Core.HashMap Types.SupplementaryConfigurationName Types.SupplementaryConfigurationValue)
    -- ^ Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the configuration parameter.
  , version :: Core.Maybe Types.Version
    -- ^ The version number of the resource configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BaseConfigurationItem' value with any optional fields omitted.
mkBaseConfigurationItem
    :: BaseConfigurationItem
mkBaseConfigurationItem
  = BaseConfigurationItem'{accountId = Core.Nothing,
                           arn = Core.Nothing, availabilityZone = Core.Nothing,
                           awsRegion = Core.Nothing, configuration = Core.Nothing,
                           configurationItemCaptureTime = Core.Nothing,
                           configurationItemStatus = Core.Nothing,
                           configurationStateId = Core.Nothing,
                           resourceCreationTime = Core.Nothing, resourceId = Core.Nothing,
                           resourceName = Core.Nothing, resourceType = Core.Nothing,
                           supplementaryConfiguration = Core.Nothing, version = Core.Nothing}

-- | The 12-digit AWS account ID associated with the resource.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAccountId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.AccountId)
bciAccountId = Lens.field @"accountId"
{-# INLINEABLE bciAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciArn :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ARN)
bciArn = Lens.field @"arn"
{-# INLINEABLE bciArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The Availability Zone associated with the resource.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAvailabilityZone :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.AvailabilityZone)
bciAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE bciAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The region where the resource resides.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciAwsRegion :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.AwsRegion)
bciAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE bciAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | The description of the resource configuration.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfiguration :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.Configuration)
bciConfiguration = Lens.field @"configuration"
{-# INLINEABLE bciConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The time when the configuration recording was initiated.
--
-- /Note:/ Consider using 'configurationItemCaptureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfigurationItemCaptureTime :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.NominalDiffTime)
bciConfigurationItemCaptureTime = Lens.field @"configurationItemCaptureTime"
{-# INLINEABLE bciConfigurationItemCaptureTime #-}
{-# DEPRECATED configurationItemCaptureTime "Use generic-lens or generic-optics with 'configurationItemCaptureTime' instead"  #-}

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
bciConfigurationItemStatus :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ConfigurationItemStatus)
bciConfigurationItemStatus = Lens.field @"configurationItemStatus"
{-# INLINEABLE bciConfigurationItemStatus #-}
{-# DEPRECATED configurationItemStatus "Use generic-lens or generic-optics with 'configurationItemStatus' instead"  #-}

-- | An identifier that indicates the ordering of the configuration items of a resource.
--
-- /Note:/ Consider using 'configurationStateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciConfigurationStateId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ConfigurationStateId)
bciConfigurationStateId = Lens.field @"configurationStateId"
{-# INLINEABLE bciConfigurationStateId #-}
{-# DEPRECATED configurationStateId "Use generic-lens or generic-optics with 'configurationStateId' instead"  #-}

-- | The time stamp when the resource was created.
--
-- /Note:/ Consider using 'resourceCreationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceCreationTime :: Lens.Lens' BaseConfigurationItem (Core.Maybe Core.NominalDiffTime)
bciResourceCreationTime = Lens.field @"resourceCreationTime"
{-# INLINEABLE bciResourceCreationTime #-}
{-# DEPRECATED resourceCreationTime "Use generic-lens or generic-optics with 'resourceCreationTime' instead"  #-}

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceId :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ResourceId)
bciResourceId = Lens.field @"resourceId"
{-# INLINEABLE bciResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The custom name of the resource, if available.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceName :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ResourceName)
bciResourceName = Lens.field @"resourceName"
{-# INLINEABLE bciResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The type of AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciResourceType :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.ResourceType)
bciResourceType = Lens.field @"resourceType"
{-# INLINEABLE bciResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the configuration parameter.
--
-- /Note:/ Consider using 'supplementaryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciSupplementaryConfiguration :: Lens.Lens' BaseConfigurationItem (Core.Maybe (Core.HashMap Types.SupplementaryConfigurationName Types.SupplementaryConfigurationValue))
bciSupplementaryConfiguration = Lens.field @"supplementaryConfiguration"
{-# INLINEABLE bciSupplementaryConfiguration #-}
{-# DEPRECATED supplementaryConfiguration "Use generic-lens or generic-optics with 'supplementaryConfiguration' instead"  #-}

-- | The version number of the resource configuration.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciVersion :: Lens.Lens' BaseConfigurationItem (Core.Maybe Types.Version)
bciVersion = Lens.field @"version"
{-# INLINEABLE bciVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON BaseConfigurationItem where
        parseJSON
          = Core.withObject "BaseConfigurationItem" Core.$
              \ x ->
                BaseConfigurationItem' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "availabilityZone"
                    Core.<*> x Core..:? "awsRegion"
                    Core.<*> x Core..:? "configuration"
                    Core.<*> x Core..:? "configurationItemCaptureTime"
                    Core.<*> x Core..:? "configurationItemStatus"
                    Core.<*> x Core..:? "configurationStateId"
                    Core.<*> x Core..:? "resourceCreationTime"
                    Core.<*> x Core..:? "resourceId"
                    Core.<*> x Core..:? "resourceName"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "supplementaryConfiguration"
                    Core.<*> x Core..:? "version"
