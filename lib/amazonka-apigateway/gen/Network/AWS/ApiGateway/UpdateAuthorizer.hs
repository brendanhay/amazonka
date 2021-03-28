{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Authorizer' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/update-authorizer.html AWS CLI> 
module Network.AWS.ApiGateway.UpdateAuthorizer
    (
    -- * Creating a request
      UpdateAuthorizer (..)
    , mkUpdateAuthorizer
    -- ** Request lenses
    , uRestApiId
    , uAuthorizerId
    , uPatchOperations

     -- * Destructuring the response
    , Types.Authorizer (..)
    , Types.mkAuthorizer
    -- ** Response lenses
    , Types.aAuthType
    , Types.aAuthorizerCredentials
    , Types.aAuthorizerResultTtlInSeconds
    , Types.aAuthorizerUri
    , Types.aId
    , Types.aIdentitySource
    , Types.aIdentityValidationExpression
    , Types.aName
    , Types.aProviderARNs
    , Types.aType
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing 'Authorizer' resource.
--
-- /See:/ 'mkUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , authorizerId :: Core.Text
    -- ^ [Required] The identifier of the 'Authorizer' resource.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAuthorizer' value with any optional fields omitted.
mkUpdateAuthorizer
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'authorizerId'
    -> UpdateAuthorizer
mkUpdateAuthorizer restApiId authorizerId
  = UpdateAuthorizer'{restApiId, authorizerId,
                      patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRestApiId :: Lens.Lens' UpdateAuthorizer Core.Text
uRestApiId = Lens.field @"restApiId"
{-# INLINEABLE uRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Authorizer' resource.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthorizerId :: Lens.Lens' UpdateAuthorizer Core.Text
uAuthorizerId = Lens.field @"authorizerId"
{-# INLINEABLE uAuthorizerId #-}
{-# DEPRECATED authorizerId "Use generic-lens or generic-optics with 'authorizerId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPatchOperations :: Lens.Lens' UpdateAuthorizer (Core.Maybe [Types.PatchOperation])
uPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAuthorizer where
        toHeaders UpdateAuthorizer{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateAuthorizer where
        toJSON UpdateAuthorizer{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateAuthorizer where
        type Rs UpdateAuthorizer = Types.Authorizer
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/authorizers/"
                             Core.<> Core.toText authorizerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
