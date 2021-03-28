{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizer' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html AWS CLI> 
module Network.AWS.ApiGateway.GetAuthorizer
    (
    -- * Creating a request
      GetAuthorizer (..)
    , mkGetAuthorizer
    -- ** Request lenses
    , gafRestApiId
    , gafAuthorizerId

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

-- | Request to describe an existing 'Authorizer' resource.
--
-- /See:/ 'mkGetAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , authorizerId :: Core.Text
    -- ^ [Required] The identifier of the 'Authorizer' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAuthorizer' value with any optional fields omitted.
mkGetAuthorizer
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'authorizerId'
    -> GetAuthorizer
mkGetAuthorizer restApiId authorizerId
  = GetAuthorizer'{restApiId, authorizerId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gafRestApiId :: Lens.Lens' GetAuthorizer Core.Text
gafRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gafRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Authorizer' resource.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gafAuthorizerId :: Lens.Lens' GetAuthorizer Core.Text
gafAuthorizerId = Lens.field @"authorizerId"
{-# INLINEABLE gafAuthorizerId #-}
{-# DEPRECATED authorizerId "Use generic-lens or generic-optics with 'authorizerId' instead"  #-}

instance Core.ToQuery GetAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAuthorizer where
        toHeaders GetAuthorizer{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetAuthorizer where
        type Rs GetAuthorizer = Types.Authorizer
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/authorizers/"
                             Core.<> Core.toText authorizerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
