{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstancesFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more new instances from a manual or automatic snapshot of an instance.
--
-- The @create instances from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @instance snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstancesFromSnapshot
    (
    -- * Creating a request
      CreateInstancesFromSnapshot (..)
    , mkCreateInstancesFromSnapshot
    -- ** Request lenses
    , cifsInstanceNames
    , cifsAvailabilityZone
    , cifsBundleId
    , cifsAddOns
    , cifsAttachedDiskMapping
    , cifsInstanceSnapshotName
    , cifsKeyPairName
    , cifsRestoreDate
    , cifsSourceInstanceName
    , cifsTags
    , cifsUseLatestRestorableAutoSnapshot
    , cifsUserData

    -- * Destructuring the response
    , CreateInstancesFromSnapshotResponse (..)
    , mkCreateInstancesFromSnapshotResponse
    -- ** Response lenses
    , cifsrrsOperations
    , cifsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstancesFromSnapshot' smart constructor.
data CreateInstancesFromSnapshot = CreateInstancesFromSnapshot'
  { instanceNames :: [Core.Text]
    -- ^ The names for your new instances.
  , availabilityZone :: Core.Text
    -- ^ The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
  , bundleId :: Types.BundleId
    -- ^ The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
  , addOns :: Core.Maybe [Types.AddOnRequest]
    -- ^ An array of objects representing the add-ons to enable for the new instance.
  , attachedDiskMapping :: Core.Maybe (Core.HashMap Types.ResourceName [Types.DiskMap])
    -- ^ An object containing information about one or more disk mappings.
  , instanceSnapshotName :: Core.Maybe Types.ResourceName
    -- ^ The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source instance name@ parameter. The @instance snapshot name@ and @source instance name@ parameters are mutually exclusive.
--
--
  , keyPairName :: Core.Maybe Types.ResourceName
    -- ^ The name for your key pair.
  , restoreDate :: Core.Maybe Core.Text
    -- ^ The date of the automatic snapshot to use for the new instance. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
  , sourceInstanceName :: Core.Maybe Core.Text
    -- ^ The name of the source instance from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @instance snapshot name@ parameter. The @source instance name@ and @instance snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  , useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool
    -- ^ A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
  , userData :: Core.Maybe Core.Text
    -- ^ You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstancesFromSnapshot' value with any optional fields omitted.
mkCreateInstancesFromSnapshot
    :: Core.Text -- ^ 'availabilityZone'
    -> Types.BundleId -- ^ 'bundleId'
    -> CreateInstancesFromSnapshot
mkCreateInstancesFromSnapshot availabilityZone bundleId
  = CreateInstancesFromSnapshot'{instanceNames = Core.mempty,
                                 availabilityZone, bundleId, addOns = Core.Nothing,
                                 attachedDiskMapping = Core.Nothing,
                                 instanceSnapshotName = Core.Nothing, keyPairName = Core.Nothing,
                                 restoreDate = Core.Nothing, sourceInstanceName = Core.Nothing,
                                 tags = Core.Nothing,
                                 useLatestRestorableAutoSnapshot = Core.Nothing,
                                 userData = Core.Nothing}

-- | The names for your new instances.
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsInstanceNames :: Lens.Lens' CreateInstancesFromSnapshot [Core.Text]
cifsInstanceNames = Lens.field @"instanceNames"
{-# INLINEABLE cifsInstanceNames #-}
{-# DEPRECATED instanceNames "Use generic-lens or generic-optics with 'instanceNames' instead"  #-}

-- | The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAvailabilityZone :: Lens.Lens' CreateInstancesFromSnapshot Core.Text
cifsAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cifsAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsBundleId :: Lens.Lens' CreateInstancesFromSnapshot Types.BundleId
cifsBundleId = Lens.field @"bundleId"
{-# INLINEABLE cifsBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | An array of objects representing the add-ons to enable for the new instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAddOns :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe [Types.AddOnRequest])
cifsAddOns = Lens.field @"addOns"
{-# INLINEABLE cifsAddOns #-}
{-# DEPRECATED addOns "Use generic-lens or generic-optics with 'addOns' instead"  #-}

-- | An object containing information about one or more disk mappings.
--
-- /Note:/ Consider using 'attachedDiskMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsAttachedDiskMapping :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe (Core.HashMap Types.ResourceName [Types.DiskMap]))
cifsAttachedDiskMapping = Lens.field @"attachedDiskMapping"
{-# INLINEABLE cifsAttachedDiskMapping #-}
{-# DEPRECATED attachedDiskMapping "Use generic-lens or generic-optics with 'attachedDiskMapping' instead"  #-}

-- | The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source instance name@ parameter. The @instance snapshot name@ and @source instance name@ parameters are mutually exclusive.
--
--
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsInstanceSnapshotName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Types.ResourceName)
cifsInstanceSnapshotName = Lens.field @"instanceSnapshotName"
{-# INLINEABLE cifsInstanceSnapshotName #-}
{-# DEPRECATED instanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead"  #-}

-- | The name for your key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsKeyPairName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Types.ResourceName)
cifsKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE cifsKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

-- | The date of the automatic snapshot to use for the new instance. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsRestoreDate :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
cifsRestoreDate = Lens.field @"restoreDate"
{-# INLINEABLE cifsRestoreDate #-}
{-# DEPRECATED restoreDate "Use generic-lens or generic-optics with 'restoreDate' instead"  #-}

-- | The name of the source instance from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @instance snapshot name@ parameter. The @source instance name@ and @instance snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsSourceInstanceName :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
cifsSourceInstanceName = Lens.field @"sourceInstanceName"
{-# INLINEABLE cifsSourceInstanceName #-}
{-# DEPRECATED sourceInstanceName "Use generic-lens or generic-optics with 'sourceInstanceName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsTags :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe [Types.Tag])
cifsTags = Lens.field @"tags"
{-# INLINEABLE cifsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new instance from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsUseLatestRestorableAutoSnapshot :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Bool)
cifsUseLatestRestorableAutoSnapshot = Lens.field @"useLatestRestorableAutoSnapshot"
{-# INLINEABLE cifsUseLatestRestorableAutoSnapshot #-}
{-# DEPRECATED useLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead"  #-}

-- | You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsUserData :: Lens.Lens' CreateInstancesFromSnapshot (Core.Maybe Core.Text)
cifsUserData = Lens.field @"userData"
{-# INLINEABLE cifsUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery CreateInstancesFromSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateInstancesFromSnapshot where
        toHeaders CreateInstancesFromSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateInstancesFromSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateInstancesFromSnapshot where
        toJSON CreateInstancesFromSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceNames" Core..= instanceNames),
                  Core.Just ("availabilityZone" Core..= availabilityZone),
                  Core.Just ("bundleId" Core..= bundleId),
                  ("addOns" Core..=) Core.<$> addOns,
                  ("attachedDiskMapping" Core..=) Core.<$> attachedDiskMapping,
                  ("instanceSnapshotName" Core..=) Core.<$> instanceSnapshotName,
                  ("keyPairName" Core..=) Core.<$> keyPairName,
                  ("restoreDate" Core..=) Core.<$> restoreDate,
                  ("sourceInstanceName" Core..=) Core.<$> sourceInstanceName,
                  ("tags" Core..=) Core.<$> tags,
                  ("useLatestRestorableAutoSnapshot" Core..=) Core.<$>
                    useLatestRestorableAutoSnapshot,
                  ("userData" Core..=) Core.<$> userData])

instance Core.AWSRequest CreateInstancesFromSnapshot where
        type Rs CreateInstancesFromSnapshot =
             CreateInstancesFromSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateInstancesFromSnapshotResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateInstancesFromSnapshotResponse' smart constructor.
data CreateInstancesFromSnapshotResponse = CreateInstancesFromSnapshotResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateInstancesFromSnapshotResponse' value with any optional fields omitted.
mkCreateInstancesFromSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInstancesFromSnapshotResponse
mkCreateInstancesFromSnapshotResponse responseStatus
  = CreateInstancesFromSnapshotResponse'{operations = Core.Nothing,
                                         responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsrrsOperations :: Lens.Lens' CreateInstancesFromSnapshotResponse (Core.Maybe [Types.Operation])
cifsrrsOperations = Lens.field @"operations"
{-# INLINEABLE cifsrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifsrrsResponseStatus :: Lens.Lens' CreateInstancesFromSnapshotResponse Core.Int
cifsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cifsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
