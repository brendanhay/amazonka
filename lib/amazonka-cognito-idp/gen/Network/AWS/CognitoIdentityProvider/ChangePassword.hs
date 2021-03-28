{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ChangePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for a specified user in a user pool.
module Network.AWS.CognitoIdentityProvider.ChangePassword
    (
    -- * Creating a request
      ChangePassword (..)
    , mkChangePassword
    -- ** Request lenses
    , cpPreviousPassword
    , cpProposedPassword
    , cpAccessToken

    -- * Destructuring the response
    , ChangePasswordResponse (..)
    , mkChangePasswordResponse
    -- ** Response lenses
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to change a user password.
--
-- /See:/ 'mkChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { previousPassword :: Types.PasswordType
    -- ^ The old password.
  , proposedPassword :: Types.PasswordType
    -- ^ The new password.
  , accessToken :: Types.AccessToken
    -- ^ The access token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePassword' value with any optional fields omitted.
mkChangePassword
    :: Types.PasswordType -- ^ 'previousPassword'
    -> Types.PasswordType -- ^ 'proposedPassword'
    -> Types.AccessToken -- ^ 'accessToken'
    -> ChangePassword
mkChangePassword previousPassword proposedPassword accessToken
  = ChangePassword'{previousPassword, proposedPassword, accessToken}

-- | The old password.
--
-- /Note:/ Consider using 'previousPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPreviousPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpPreviousPassword = Lens.field @"previousPassword"
{-# INLINEABLE cpPreviousPassword #-}
{-# DEPRECATED previousPassword "Use generic-lens or generic-optics with 'previousPassword' instead"  #-}

-- | The new password.
--
-- /Note:/ Consider using 'proposedPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProposedPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpProposedPassword = Lens.field @"proposedPassword"
{-# INLINEABLE cpProposedPassword #-}
{-# DEPRECATED proposedPassword "Use generic-lens or generic-optics with 'proposedPassword' instead"  #-}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAccessToken :: Lens.Lens' ChangePassword Types.AccessToken
cpAccessToken = Lens.field @"accessToken"
{-# INLINEABLE cpAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

instance Core.ToQuery ChangePassword where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ChangePassword where
        toHeaders ChangePassword{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.ChangePassword")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ChangePassword where
        toJSON ChangePassword{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PreviousPassword" Core..= previousPassword),
                  Core.Just ("ProposedPassword" Core..= proposedPassword),
                  Core.Just ("AccessToken" Core..= accessToken)])

instance Core.AWSRequest ChangePassword where
        type Rs ChangePassword = ChangePasswordResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ChangePasswordResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The response from the server to the change password request.
--
-- /See:/ 'mkChangePasswordResponse' smart constructor.
newtype ChangePasswordResponse = ChangePasswordResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePasswordResponse' value with any optional fields omitted.
mkChangePasswordResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ChangePasswordResponse
mkChangePasswordResponse responseStatus
  = ChangePasswordResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' ChangePasswordResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
