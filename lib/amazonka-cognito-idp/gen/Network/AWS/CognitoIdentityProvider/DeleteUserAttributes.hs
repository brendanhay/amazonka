{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the attributes for a user.
module Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
    (
    -- * Creating a request
      DeleteUserAttributes (..)
    , mkDeleteUserAttributes
    -- ** Request lenses
    , duaUserAttributeNames
    , duaAccessToken

    -- * Destructuring the response
    , DeleteUserAttributesResponse (..)
    , mkDeleteUserAttributesResponse
    -- ** Response lenses
    , duarrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete user attributes.
--
-- /See:/ 'mkDeleteUserAttributes' smart constructor.
data DeleteUserAttributes = DeleteUserAttributes'
  { userAttributeNames :: [Types.AttributeNameType]
    -- ^ An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
  , accessToken :: Types.TokenModelType
    -- ^ The access token used in the request to delete user attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserAttributes' value with any optional fields omitted.
mkDeleteUserAttributes
    :: Types.TokenModelType -- ^ 'accessToken'
    -> DeleteUserAttributes
mkDeleteUserAttributes accessToken
  = DeleteUserAttributes'{userAttributeNames = Core.mempty,
                          accessToken}

-- | An array of strings representing the user attribute names you wish to delete.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaUserAttributeNames :: Lens.Lens' DeleteUserAttributes [Types.AttributeNameType]
duaUserAttributeNames = Lens.field @"userAttributeNames"
{-# INLINEABLE duaUserAttributeNames #-}
{-# DEPRECATED userAttributeNames "Use generic-lens or generic-optics with 'userAttributeNames' instead"  #-}

-- | The access token used in the request to delete user attributes.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duaAccessToken :: Lens.Lens' DeleteUserAttributes Types.TokenModelType
duaAccessToken = Lens.field @"accessToken"
{-# INLINEABLE duaAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

instance Core.ToQuery DeleteUserAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserAttributes where
        toHeaders DeleteUserAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DeleteUserAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserAttributes where
        toJSON DeleteUserAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserAttributeNames" Core..= userAttributeNames),
                  Core.Just ("AccessToken" Core..= accessToken)])

instance Core.AWSRequest DeleteUserAttributes where
        type Rs DeleteUserAttributes = DeleteUserAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteUserAttributesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to delete user attributes.
--
-- /See:/ 'mkDeleteUserAttributesResponse' smart constructor.
newtype DeleteUserAttributesResponse = DeleteUserAttributesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserAttributesResponse' value with any optional fields omitted.
mkDeleteUserAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteUserAttributesResponse
mkDeleteUserAttributesResponse responseStatus
  = DeleteUserAttributesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duarrsResponseStatus :: Lens.Lens' DeleteUserAttributesResponse Core.Int
duarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
