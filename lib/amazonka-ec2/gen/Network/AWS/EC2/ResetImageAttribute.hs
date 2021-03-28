{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an AMI to its default value.
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Creating a request
      ResetImageAttribute (..)
    , mkResetImageAttribute
    -- ** Request lenses
    , riafAttribute
    , riafImageId
    , riafDryRun

    -- * Destructuring the response
    , ResetImageAttributeResponse (..)
    , mkResetImageAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ResetImageAttribute.
--
-- /See:/ 'mkResetImageAttribute' smart constructor.
data ResetImageAttribute = ResetImageAttribute'
  { attribute :: Types.ResetImageAttributeName
    -- ^ The attribute to reset (currently you can only reset the launch permission attribute).
  , imageId :: Types.ImageId
    -- ^ The ID of the AMI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetImageAttribute' value with any optional fields omitted.
mkResetImageAttribute
    :: Types.ResetImageAttributeName -- ^ 'attribute'
    -> Types.ImageId -- ^ 'imageId'
    -> ResetImageAttribute
mkResetImageAttribute attribute imageId
  = ResetImageAttribute'{attribute, imageId, dryRun = Core.Nothing}

-- | The attribute to reset (currently you can only reset the launch permission attribute).
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafAttribute :: Lens.Lens' ResetImageAttribute Types.ResetImageAttributeName
riafAttribute = Lens.field @"attribute"
{-# INLINEABLE riafAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafImageId :: Lens.Lens' ResetImageAttribute Types.ImageId
riafImageId = Lens.field @"imageId"
{-# INLINEABLE riafImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riafDryRun :: Lens.Lens' ResetImageAttribute (Core.Maybe Core.Bool)
riafDryRun = Lens.field @"dryRun"
{-# INLINEABLE riafDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ResetImageAttribute where
        toQuery ResetImageAttribute{..}
          = Core.toQueryPair "Action" ("ResetImageAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "ImageId" imageId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ResetImageAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetImageAttribute where
        type Rs ResetImageAttribute = ResetImageAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ResetImageAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetImageAttributeResponse' smart constructor.
data ResetImageAttributeResponse = ResetImageAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetImageAttributeResponse' value with any optional fields omitted.
mkResetImageAttributeResponse
    :: ResetImageAttributeResponse
mkResetImageAttributeResponse = ResetImageAttributeResponse'
