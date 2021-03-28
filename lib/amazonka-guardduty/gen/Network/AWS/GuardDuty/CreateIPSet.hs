{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IPSet, which is called a trusted IP list in the console user interface. An IPSet is a list of IP addresses that are trusted for secure communication with AWS infrastructure and applications. GuardDuty doesn't generate findings for IP addresses that are included in IPSets. Only users from the master account can use this operation.
module Network.AWS.GuardDuty.CreateIPSet
    (
    -- * Creating a request
      CreateIPSet (..)
    , mkCreateIPSet
    -- ** Request lenses
    , cipsDetectorId
    , cipsName
    , cipsFormat
    , cipsLocation
    , cipsActivate
    , cipsClientToken
    , cipsTags

    -- * Destructuring the response
    , CreateIPSetResponse (..)
    , mkCreateIPSetResponse
    -- ** Response lenses
    , cipsrrsIpSetId
    , cipsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector of the GuardDuty account that you want to create an IPSet for.
  , name :: Types.Name
    -- ^ The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and underscores (_).
  , format :: Types.IpSetFormat
    -- ^ The format of the file that contains the IPSet.
  , location :: Types.Location
    -- ^ The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
  , activate :: Core.Bool
    -- ^ A Boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ The idempotency token for the create request.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to be added to a new IP set resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIPSet' value with any optional fields omitted.
mkCreateIPSet
    :: Types.DetectorId -- ^ 'detectorId'
    -> Types.Name -- ^ 'name'
    -> Types.IpSetFormat -- ^ 'format'
    -> Types.Location -- ^ 'location'
    -> Core.Bool -- ^ 'activate'
    -> CreateIPSet
mkCreateIPSet detectorId name format location activate
  = CreateIPSet'{detectorId, name, format, location, activate,
                 clientToken = Core.Nothing, tags = Core.Nothing}

-- | The unique ID of the detector of the GuardDuty account that you want to create an IPSet for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsDetectorId :: Lens.Lens' CreateIPSet Types.DetectorId
cipsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE cipsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and underscores (_).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsName :: Lens.Lens' CreateIPSet Types.Name
cipsName = Lens.field @"name"
{-# INLINEABLE cipsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The format of the file that contains the IPSet.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsFormat :: Lens.Lens' CreateIPSet Types.IpSetFormat
cipsFormat = Lens.field @"format"
{-# INLINEABLE cipsFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsLocation :: Lens.Lens' CreateIPSet Types.Location
cipsLocation = Lens.field @"location"
{-# INLINEABLE cipsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | A Boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsActivate :: Lens.Lens' CreateIPSet Core.Bool
cipsActivate = Lens.field @"activate"
{-# INLINEABLE cipsActivate #-}
{-# DEPRECATED activate "Use generic-lens or generic-optics with 'activate' instead"  #-}

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsClientToken :: Lens.Lens' CreateIPSet (Core.Maybe Types.ClientToken)
cipsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cipsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The tags to be added to a new IP set resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsTags :: Lens.Lens' CreateIPSet (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cipsTags = Lens.field @"tags"
{-# INLINEABLE cipsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateIPSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateIPSet where
        toHeaders CreateIPSet{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateIPSet where
        toJSON CreateIPSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("format" Core..= format),
                  Core.Just ("location" Core..= location),
                  Core.Just ("activate" Core..= activate),
                  ("clientToken" Core..=) Core.<$> clientToken,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateIPSet where
        type Rs CreateIPSet = CreateIPSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/ipset",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateIPSetResponse' Core.<$>
                   (x Core..: "ipSetId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { ipSetId :: Core.Text
    -- ^ The ID of the IPSet resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIPSetResponse' value with any optional fields omitted.
mkCreateIPSetResponse
    :: Core.Text -- ^ 'ipSetId'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateIPSetResponse
mkCreateIPSetResponse ipSetId responseStatus
  = CreateIPSetResponse'{ipSetId, responseStatus}

-- | The ID of the IPSet resource.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsrrsIpSetId :: Lens.Lens' CreateIPSetResponse Core.Text
cipsrrsIpSetId = Lens.field @"ipSetId"
{-# INLINEABLE cipsrrsIpSetId #-}
{-# DEPRECATED ipSetId "Use generic-lens or generic-optics with 'ipSetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipsrrsResponseStatus :: Lens.Lens' CreateIPSetResponse Core.Int
cipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
