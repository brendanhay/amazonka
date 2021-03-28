{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroupCertificateAuthorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current CAs for a group.
module Network.AWS.Greengrass.ListGroupCertificateAuthorities
    (
    -- * Creating a request
      ListGroupCertificateAuthorities (..)
    , mkListGroupCertificateAuthorities
    -- ** Request lenses
    , lgcaGroupId

    -- * Destructuring the response
    , ListGroupCertificateAuthoritiesResponse (..)
    , mkListGroupCertificateAuthoritiesResponse
    -- ** Response lenses
    , lgcarrsGroupCertificateAuthorities
    , lgcarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroupCertificateAuthorities' smart constructor.
newtype ListGroupCertificateAuthorities = ListGroupCertificateAuthorities'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupCertificateAuthorities' value with any optional fields omitted.
mkListGroupCertificateAuthorities
    :: Core.Text -- ^ 'groupId'
    -> ListGroupCertificateAuthorities
mkListGroupCertificateAuthorities groupId
  = ListGroupCertificateAuthorities'{groupId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcaGroupId :: Lens.Lens' ListGroupCertificateAuthorities Core.Text
lgcaGroupId = Lens.field @"groupId"
{-# INLINEABLE lgcaGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery ListGroupCertificateAuthorities where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListGroupCertificateAuthorities where
        toHeaders ListGroupCertificateAuthorities{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListGroupCertificateAuthorities where
        type Rs ListGroupCertificateAuthorities =
             ListGroupCertificateAuthoritiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/certificateauthorities",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGroupCertificateAuthoritiesResponse' Core.<$>
                   (x Core..:? "GroupCertificateAuthorities") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListGroupCertificateAuthoritiesResponse' smart constructor.
data ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse'
  { groupCertificateAuthorities :: Core.Maybe [Types.GroupCertificateAuthorityProperties]
    -- ^ A list of certificate authorities associated with the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupCertificateAuthoritiesResponse' value with any optional fields omitted.
mkListGroupCertificateAuthoritiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGroupCertificateAuthoritiesResponse
mkListGroupCertificateAuthoritiesResponse responseStatus
  = ListGroupCertificateAuthoritiesResponse'{groupCertificateAuthorities
                                               = Core.Nothing,
                                             responseStatus}

-- | A list of certificate authorities associated with the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcarrsGroupCertificateAuthorities :: Lens.Lens' ListGroupCertificateAuthoritiesResponse (Core.Maybe [Types.GroupCertificateAuthorityProperties])
lgcarrsGroupCertificateAuthorities = Lens.field @"groupCertificateAuthorities"
{-# INLINEABLE lgcarrsGroupCertificateAuthorities #-}
{-# DEPRECATED groupCertificateAuthorities "Use generic-lens or generic-optics with 'groupCertificateAuthorities' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgcarrsResponseStatus :: Lens.Lens' ListGroupCertificateAuthoritiesResponse Core.Int
lgcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
