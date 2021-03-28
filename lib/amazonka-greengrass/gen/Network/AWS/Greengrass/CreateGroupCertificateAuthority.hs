{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateGroupCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CA for the group. If a CA already exists, it will rotate the existing CA.
module Network.AWS.Greengrass.CreateGroupCertificateAuthority
    (
    -- * Creating a request
      CreateGroupCertificateAuthority (..)
    , mkCreateGroupCertificateAuthority
    -- ** Request lenses
    , cgcaGroupId
    , cgcaAmznClientToken

    -- * Destructuring the response
    , CreateGroupCertificateAuthorityResponse (..)
    , mkCreateGroupCertificateAuthorityResponse
    -- ** Response lenses
    , cgcarrsGroupCertificateAuthorityArn
    , cgcarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGroupCertificateAuthority' smart constructor.
data CreateGroupCertificateAuthority = CreateGroupCertificateAuthority'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupCertificateAuthority' value with any optional fields omitted.
mkCreateGroupCertificateAuthority
    :: Core.Text -- ^ 'groupId'
    -> CreateGroupCertificateAuthority
mkCreateGroupCertificateAuthority groupId
  = CreateGroupCertificateAuthority'{groupId,
                                     amznClientToken = Core.Nothing}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaGroupId :: Lens.Lens' CreateGroupCertificateAuthority Core.Text
cgcaGroupId = Lens.field @"groupId"
{-# INLINEABLE cgcaGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaAmznClientToken :: Lens.Lens' CreateGroupCertificateAuthority (Core.Maybe Core.Text)
cgcaAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE cgcaAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

instance Core.ToQuery CreateGroupCertificateAuthority where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGroupCertificateAuthority where
        toHeaders CreateGroupCertificateAuthority{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGroupCertificateAuthority where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateGroupCertificateAuthority where
        type Rs CreateGroupCertificateAuthority =
             CreateGroupCertificateAuthorityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/certificateauthorities",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGroupCertificateAuthorityResponse' Core.<$>
                   (x Core..:? "GroupCertificateAuthorityArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGroupCertificateAuthorityResponse' smart constructor.
data CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse'
  { groupCertificateAuthorityArn :: Core.Maybe Core.Text
    -- ^ The ARN of the group certificate authority.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroupCertificateAuthorityResponse' value with any optional fields omitted.
mkCreateGroupCertificateAuthorityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGroupCertificateAuthorityResponse
mkCreateGroupCertificateAuthorityResponse responseStatus
  = CreateGroupCertificateAuthorityResponse'{groupCertificateAuthorityArn
                                               = Core.Nothing,
                                             responseStatus}

-- | The ARN of the group certificate authority.
--
-- /Note:/ Consider using 'groupCertificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarrsGroupCertificateAuthorityArn :: Lens.Lens' CreateGroupCertificateAuthorityResponse (Core.Maybe Core.Text)
cgcarrsGroupCertificateAuthorityArn = Lens.field @"groupCertificateAuthorityArn"
{-# INLINEABLE cgcarrsGroupCertificateAuthorityArn #-}
{-# DEPRECATED groupCertificateAuthorityArn "Use generic-lens or generic-optics with 'groupCertificateAuthorityArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarrsResponseStatus :: Lens.Lens' CreateGroupCertificateAuthorityResponse Core.Int
cgcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
