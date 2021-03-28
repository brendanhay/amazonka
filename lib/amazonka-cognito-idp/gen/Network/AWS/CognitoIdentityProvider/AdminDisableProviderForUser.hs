{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the user from signing in with the specified external (SAML or social) identity provider. If the user to disable is a Cognito User Pools native username + password user, they are not permitted to use their password to sign-in. If the user to disable is a linked external IdP user, any link between that user and an existing user is removed. The next time the external user (no longer attached to the previously linked @DestinationUser@ ) signs in, they must create a new user account. See <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminLinkProviderForUser.html AdminLinkProviderForUser> .
--
-- This action is enabled only for admin access and requires developer credentials.
-- The @ProviderName@ must match the value specified when creating an IdP for the pool. 
-- To disable a native username + password user, the @ProviderName@ value must be @Cognito@ and the @ProviderAttributeName@ must be @Cognito_Subject@ , with the @ProviderAttributeValue@ being the name that is used in the user pool for the user.
-- The @ProviderAttributeName@ must always be @Cognito_Subject@ for social identity providers. The @ProviderAttributeValue@ must always be the exact subject that was used when the user was originally linked as a source user.
-- For de-linking a SAML identity, there are two scenarios. If the linked identity has not yet been used to sign-in, the @ProviderAttributeName@ and @ProviderAttributeValue@ must be the same values that were used for the @SourceUser@ when the identities were originally linked using @AdminLinkProviderForUser@ call. (If the linking was done with @ProviderAttributeName@ set to @Cognito_Subject@ , the same applies here). However, if the user has already signed in, the @ProviderAttributeName@ must be @Cognito_Subject@ and @ProviderAttributeValue@ must be the subject of the SAML assertion.
module Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
    (
    -- * Creating a request
      AdminDisableProviderForUser (..)
    , mkAdminDisableProviderForUser
    -- ** Request lenses
    , adpfuUserPoolId
    , adpfuUser

    -- * Destructuring the response
    , AdminDisableProviderForUserResponse (..)
    , mkAdminDisableProviderForUserResponse
    -- ** Response lenses
    , adpfurrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminDisableProviderForUser' smart constructor.
data AdminDisableProviderForUser = AdminDisableProviderForUser'
  { userPoolId :: Types.StringType
    -- ^ The user pool ID for the user pool.
  , user :: Types.ProviderUserIdentifierType
    -- ^ The user to be disabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDisableProviderForUser' value with any optional fields omitted.
mkAdminDisableProviderForUser
    :: Types.StringType -- ^ 'userPoolId'
    -> Types.ProviderUserIdentifierType -- ^ 'user'
    -> AdminDisableProviderForUser
mkAdminDisableProviderForUser userPoolId user
  = AdminDisableProviderForUser'{userPoolId, user}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfuUserPoolId :: Lens.Lens' AdminDisableProviderForUser Types.StringType
adpfuUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE adpfuUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user to be disabled.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfuUser :: Lens.Lens' AdminDisableProviderForUser Types.ProviderUserIdentifierType
adpfuUser = Lens.field @"user"
{-# INLINEABLE adpfuUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

instance Core.ToQuery AdminDisableProviderForUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminDisableProviderForUser where
        toHeaders AdminDisableProviderForUser{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminDisableProviderForUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminDisableProviderForUser where
        toJSON AdminDisableProviderForUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("User" Core..= user)])

instance Core.AWSRequest AdminDisableProviderForUser where
        type Rs AdminDisableProviderForUser =
             AdminDisableProviderForUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminDisableProviderForUserResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminDisableProviderForUserResponse' smart constructor.
newtype AdminDisableProviderForUserResponse = AdminDisableProviderForUserResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminDisableProviderForUserResponse' value with any optional fields omitted.
mkAdminDisableProviderForUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminDisableProviderForUserResponse
mkAdminDisableProviderForUserResponse responseStatus
  = AdminDisableProviderForUserResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adpfurrsResponseStatus :: Lens.Lens' AdminDisableProviderForUserResponse Core.Int
adpfurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adpfurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
