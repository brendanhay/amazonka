{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more Amazon Lightsail instances.
--
-- The @create instances@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstances
    (
    -- * Creating a request
      CreateInstances (..)
    , mkCreateInstances
    -- ** Request lenses
    , ciInstanceNames
    , ciAvailabilityZone
    , ciBlueprintId
    , ciBundleId
    , ciAddOns
    , ciCustomImageName
    , ciKeyPairName
    , ciTags
    , ciUserData

    -- * Destructuring the response
    , CreateInstancesResponse (..)
    , mkCreateInstancesResponse
    -- ** Response lenses
    , cirrsOperations
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstances' smart constructor.
data CreateInstances = CreateInstances'
  { instanceNames :: [Core.Text]
    -- ^ The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@ 
  , availabilityZone :: Core.Text
    -- ^ The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
  , blueprintId :: Types.NonEmptyString
    -- ^ The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
  , bundleId :: Types.NonEmptyString
    -- ^ The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
  , addOns :: Core.Maybe [Types.AddOnRequest]
    -- ^ An array of objects representing the add-ons to enable for the new instance.
  , customImageName :: Core.Maybe Types.ResourceName
    -- ^ (Deprecated) The name for your custom image.
  , keyPairName :: Core.Maybe Types.ResourceName
    -- ^ The name of your key pair.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  , userData :: Core.Maybe Core.Text
    -- ^ A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstances' value with any optional fields omitted.
mkCreateInstances
    :: Core.Text -- ^ 'availabilityZone'
    -> Types.NonEmptyString -- ^ 'blueprintId'
    -> Types.NonEmptyString -- ^ 'bundleId'
    -> CreateInstances
mkCreateInstances availabilityZone blueprintId bundleId
  = CreateInstances'{instanceNames = Core.mempty, availabilityZone,
                     blueprintId, bundleId, addOns = Core.Nothing,
                     customImageName = Core.Nothing, keyPairName = Core.Nothing,
                     tags = Core.Nothing, userData = Core.Nothing}

-- | The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@ 
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceNames :: Lens.Lens' CreateInstances [Core.Text]
ciInstanceNames = Lens.field @"instanceNames"
{-# INLINEABLE ciInstanceNames #-}
{-# DEPRECATED instanceNames "Use generic-lens or generic-optics with 'instanceNames' instead"  #-}

-- | The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' CreateInstances Core.Text
ciAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ciAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBlueprintId :: Lens.Lens' CreateInstances Types.NonEmptyString
ciBlueprintId = Lens.field @"blueprintId"
{-# INLINEABLE ciBlueprintId #-}
{-# DEPRECATED blueprintId "Use generic-lens or generic-optics with 'blueprintId' instead"  #-}

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBundleId :: Lens.Lens' CreateInstances Types.NonEmptyString
ciBundleId = Lens.field @"bundleId"
{-# INLINEABLE ciBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | An array of objects representing the add-ons to enable for the new instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAddOns :: Lens.Lens' CreateInstances (Core.Maybe [Types.AddOnRequest])
ciAddOns = Lens.field @"addOns"
{-# INLINEABLE ciAddOns #-}
{-# DEPRECATED addOns "Use generic-lens or generic-optics with 'addOns' instead"  #-}

-- | (Deprecated) The name for your custom image.
--
-- /Note:/ Consider using 'customImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCustomImageName :: Lens.Lens' CreateInstances (Core.Maybe Types.ResourceName)
ciCustomImageName = Lens.field @"customImageName"
{-# INLINEABLE ciCustomImageName #-}
{-# DEPRECATED customImageName "Use generic-lens or generic-optics with 'customImageName' instead"  #-}

-- | The name of your key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciKeyPairName :: Lens.Lens' CreateInstances (Core.Maybe Types.ResourceName)
ciKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE ciKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInstances (Core.Maybe [Types.Tag])
ciTags = Lens.field @"tags"
{-# INLINEABLE ciTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciUserData :: Lens.Lens' CreateInstances (Core.Maybe Core.Text)
ciUserData = Lens.field @"userData"
{-# INLINEABLE ciUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery CreateInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateInstances where
        toHeaders CreateInstances{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.CreateInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateInstances where
        toJSON CreateInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceNames" Core..= instanceNames),
                  Core.Just ("availabilityZone" Core..= availabilityZone),
                  Core.Just ("blueprintId" Core..= blueprintId),
                  Core.Just ("bundleId" Core..= bundleId),
                  ("addOns" Core..=) Core.<$> addOns,
                  ("customImageName" Core..=) Core.<$> customImageName,
                  ("keyPairName" Core..=) Core.<$> keyPairName,
                  ("tags" Core..=) Core.<$> tags,
                  ("userData" Core..=) Core.<$> userData])

instance Core.AWSRequest CreateInstances where
        type Rs CreateInstances = CreateInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateInstancesResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateInstancesResponse' smart constructor.
data CreateInstancesResponse = CreateInstancesResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateInstancesResponse' value with any optional fields omitted.
mkCreateInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInstancesResponse
mkCreateInstancesResponse responseStatus
  = CreateInstancesResponse'{operations = Core.Nothing,
                             responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsOperations :: Lens.Lens' CreateInstancesResponse (Core.Maybe [Types.Operation])
cirrsOperations = Lens.field @"operations"
{-# INLINEABLE cirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInstancesResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
