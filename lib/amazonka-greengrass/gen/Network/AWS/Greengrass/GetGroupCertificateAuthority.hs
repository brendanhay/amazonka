{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroupCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retreives the CA associated with a group. Returns the public key of the CA.
module Network.AWS.Greengrass.GetGroupCertificateAuthority
    (
    -- * Creating a request
      GetGroupCertificateAuthority (..)
    , mkGetGroupCertificateAuthority
    -- ** Request lenses
    , ggcaCertificateAuthorityId
    , ggcaGroupId

    -- * Destructuring the response
    , GetGroupCertificateAuthorityResponse (..)
    , mkGetGroupCertificateAuthorityResponse
    -- ** Response lenses
    , ggcarrsGroupCertificateAuthorityArn
    , ggcarrsGroupCertificateAuthorityId
    , ggcarrsPemEncodedCertificate
    , ggcarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupCertificateAuthority' smart constructor.
data GetGroupCertificateAuthority = GetGroupCertificateAuthority'
  { certificateAuthorityId :: Core.Text
    -- ^ The ID of the certificate authority.
  , groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupCertificateAuthority' value with any optional fields omitted.
mkGetGroupCertificateAuthority
    :: Core.Text -- ^ 'certificateAuthorityId'
    -> Core.Text -- ^ 'groupId'
    -> GetGroupCertificateAuthority
mkGetGroupCertificateAuthority certificateAuthorityId groupId
  = GetGroupCertificateAuthority'{certificateAuthorityId, groupId}

-- | The ID of the certificate authority.
--
-- /Note:/ Consider using 'certificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcaCertificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthority Core.Text
ggcaCertificateAuthorityId = Lens.field @"certificateAuthorityId"
{-# INLINEABLE ggcaCertificateAuthorityId #-}
{-# DEPRECATED certificateAuthorityId "Use generic-lens or generic-optics with 'certificateAuthorityId' instead"  #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcaGroupId :: Lens.Lens' GetGroupCertificateAuthority Core.Text
ggcaGroupId = Lens.field @"groupId"
{-# INLINEABLE ggcaGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

instance Core.ToQuery GetGroupCertificateAuthority where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGroupCertificateAuthority where
        toHeaders GetGroupCertificateAuthority{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetGroupCertificateAuthority where
        type Rs GetGroupCertificateAuthority =
             GetGroupCertificateAuthorityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/certificateauthorities/"
                             Core.<> Core.toText certificateAuthorityId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGroupCertificateAuthorityResponse' Core.<$>
                   (x Core..:? "GroupCertificateAuthorityArn") Core.<*>
                     x Core..:? "GroupCertificateAuthorityId"
                     Core.<*> x Core..:? "PemEncodedCertificate"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGroupCertificateAuthorityResponse' smart constructor.
data GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse'
  { groupCertificateAuthorityArn :: Core.Maybe Core.Text
    -- ^ The ARN of the certificate authority for the group.
  , groupCertificateAuthorityId :: Core.Maybe Core.Text
    -- ^ The ID of the certificate authority for the group.
  , pemEncodedCertificate :: Core.Maybe Core.Text
    -- ^ The PEM encoded certificate for the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupCertificateAuthorityResponse' value with any optional fields omitted.
mkGetGroupCertificateAuthorityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGroupCertificateAuthorityResponse
mkGetGroupCertificateAuthorityResponse responseStatus
  = GetGroupCertificateAuthorityResponse'{groupCertificateAuthorityArn
                                            = Core.Nothing,
                                          groupCertificateAuthorityId = Core.Nothing,
                                          pemEncodedCertificate = Core.Nothing, responseStatus}

-- | The ARN of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarrsGroupCertificateAuthorityArn :: Lens.Lens' GetGroupCertificateAuthorityResponse (Core.Maybe Core.Text)
ggcarrsGroupCertificateAuthorityArn = Lens.field @"groupCertificateAuthorityArn"
{-# INLINEABLE ggcarrsGroupCertificateAuthorityArn #-}
{-# DEPRECATED groupCertificateAuthorityArn "Use generic-lens or generic-optics with 'groupCertificateAuthorityArn' instead"  #-}

-- | The ID of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarrsGroupCertificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthorityResponse (Core.Maybe Core.Text)
ggcarrsGroupCertificateAuthorityId = Lens.field @"groupCertificateAuthorityId"
{-# INLINEABLE ggcarrsGroupCertificateAuthorityId #-}
{-# DEPRECATED groupCertificateAuthorityId "Use generic-lens or generic-optics with 'groupCertificateAuthorityId' instead"  #-}

-- | The PEM encoded certificate for the group.
--
-- /Note:/ Consider using 'pemEncodedCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarrsPemEncodedCertificate :: Lens.Lens' GetGroupCertificateAuthorityResponse (Core.Maybe Core.Text)
ggcarrsPemEncodedCertificate = Lens.field @"pemEncodedCertificate"
{-# INLINEABLE ggcarrsPemEncodedCertificate #-}
{-# DEPRECATED pemEncodedCertificate "Use generic-lens or generic-optics with 'pemEncodedCertificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarrsResponseStatus :: Lens.Lens' GetGroupCertificateAuthorityResponse Core.Int
ggcarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggcarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
