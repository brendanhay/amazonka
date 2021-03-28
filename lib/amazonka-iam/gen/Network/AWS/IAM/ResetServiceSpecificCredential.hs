{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResetServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a service-specific credential. The new password is AWS generated and cryptographically strong. It cannot be configured by the user. Resetting the password immediately invalidates the previous password associated with this user.
module Network.AWS.IAM.ResetServiceSpecificCredential
    (
    -- * Creating a request
      ResetServiceSpecificCredential (..)
    , mkResetServiceSpecificCredential
    -- ** Request lenses
    , rsscServiceSpecificCredentialId
    , rsscUserName

    -- * Destructuring the response
    , ResetServiceSpecificCredentialResponse (..)
    , mkResetServiceSpecificCredentialResponse
    -- ** Response lenses
    , rsscrrsServiceSpecificCredential
    , rsscrrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetServiceSpecificCredential' smart constructor.
data ResetServiceSpecificCredential = ResetServiceSpecificCredential'
  { serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId
    -- ^ The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
  , userName :: Core.Maybe Types.UserName
    -- ^ The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetServiceSpecificCredential' value with any optional fields omitted.
mkResetServiceSpecificCredential
    :: Types.ServiceSpecificCredentialId -- ^ 'serviceSpecificCredentialId'
    -> ResetServiceSpecificCredential
mkResetServiceSpecificCredential serviceSpecificCredentialId
  = ResetServiceSpecificCredential'{serviceSpecificCredentialId,
                                    userName = Core.Nothing}

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscServiceSpecificCredentialId :: Lens.Lens' ResetServiceSpecificCredential Types.ServiceSpecificCredentialId
rsscServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# INLINEABLE rsscServiceSpecificCredentialId #-}
{-# DEPRECATED serviceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead"  #-}

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscUserName :: Lens.Lens' ResetServiceSpecificCredential (Core.Maybe Types.UserName)
rsscUserName = Lens.field @"userName"
{-# INLINEABLE rsscUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.ToQuery ResetServiceSpecificCredential where
        toQuery ResetServiceSpecificCredential{..}
          = Core.toQueryPair "Action"
              ("ResetServiceSpecificCredential" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServiceSpecificCredentialId"
                serviceSpecificCredentialId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserName") userName

instance Core.ToHeaders ResetServiceSpecificCredential where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetServiceSpecificCredential where
        type Rs ResetServiceSpecificCredential =
             ResetServiceSpecificCredentialResponse
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
        parseResponse
          = Response.receiveXMLWrapper "ResetServiceSpecificCredentialResult"
              (\ s h x ->
                 ResetServiceSpecificCredentialResponse' Core.<$>
                   (x Core..@? "ServiceSpecificCredential") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetServiceSpecificCredentialResponse' smart constructor.
data ResetServiceSpecificCredentialResponse = ResetServiceSpecificCredentialResponse'
  { serviceSpecificCredential :: Core.Maybe Types.ServiceSpecificCredential
    -- ^ A structure with details about the updated service-specific credential, including the new password.
--
-- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResetServiceSpecificCredentialResponse' value with any optional fields omitted.
mkResetServiceSpecificCredentialResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetServiceSpecificCredentialResponse
mkResetServiceSpecificCredentialResponse responseStatus
  = ResetServiceSpecificCredentialResponse'{serviceSpecificCredential
                                              = Core.Nothing,
                                            responseStatus}

-- | A structure with details about the updated service-specific credential, including the new password.
--
-- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
--
-- /Note:/ Consider using 'serviceSpecificCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrrsServiceSpecificCredential :: Lens.Lens' ResetServiceSpecificCredentialResponse (Core.Maybe Types.ServiceSpecificCredential)
rsscrrsServiceSpecificCredential = Lens.field @"serviceSpecificCredential"
{-# INLINEABLE rsscrrsServiceSpecificCredential #-}
{-# DEPRECATED serviceSpecificCredential "Use generic-lens or generic-optics with 'serviceSpecificCredential' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrrsResponseStatus :: Lens.Lens' ResetServiceSpecificCredentialResponse Core.Int
rsscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
