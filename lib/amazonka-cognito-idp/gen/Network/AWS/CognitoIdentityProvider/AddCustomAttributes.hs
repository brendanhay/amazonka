{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AddCustomAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional user attributes to the user pool schema.
module Network.AWS.CognitoIdentityProvider.AddCustomAttributes
    (
    -- * Creating a request
      AddCustomAttributes (..)
    , mkAddCustomAttributes
    -- ** Request lenses
    , acaUserPoolId
    , acaCustomAttributes

    -- * Destructuring the response
    , AddCustomAttributesResponse (..)
    , mkAddCustomAttributesResponse
    -- ** Response lenses
    , acarrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to add custom attributes.
--
-- /See:/ 'mkAddCustomAttributes' smart constructor.
data AddCustomAttributes = AddCustomAttributes'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to add custom attributes.
  , customAttributes :: Core.NonEmpty Types.SchemaAttributeType
    -- ^ An array of custom attributes, such as Mutable and Name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddCustomAttributes' value with any optional fields omitted.
mkAddCustomAttributes
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Core.NonEmpty Types.SchemaAttributeType -- ^ 'customAttributes'
    -> AddCustomAttributes
mkAddCustomAttributes userPoolId customAttributes
  = AddCustomAttributes'{userPoolId, customAttributes}

-- | The user pool ID for the user pool where you want to add custom attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaUserPoolId :: Lens.Lens' AddCustomAttributes Types.UserPoolId
acaUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE acaUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | An array of custom attributes, such as Mutable and Name.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaCustomAttributes :: Lens.Lens' AddCustomAttributes (Core.NonEmpty Types.SchemaAttributeType)
acaCustomAttributes = Lens.field @"customAttributes"
{-# INLINEABLE acaCustomAttributes #-}
{-# DEPRECATED customAttributes "Use generic-lens or generic-optics with 'customAttributes' instead"  #-}

instance Core.ToQuery AddCustomAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddCustomAttributes where
        toHeaders AddCustomAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AddCustomAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddCustomAttributes where
        toJSON AddCustomAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("CustomAttributes" Core..= customAttributes)])

instance Core.AWSRequest AddCustomAttributes where
        type Rs AddCustomAttributes = AddCustomAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddCustomAttributesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server for the request to add custom attributes.
--
-- /See:/ 'mkAddCustomAttributesResponse' smart constructor.
newtype AddCustomAttributesResponse = AddCustomAttributesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddCustomAttributesResponse' value with any optional fields omitted.
mkAddCustomAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddCustomAttributesResponse
mkAddCustomAttributesResponse responseStatus
  = AddCustomAttributesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarrsResponseStatus :: Lens.Lens' AddCustomAttributesResponse Core.Int
acarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
