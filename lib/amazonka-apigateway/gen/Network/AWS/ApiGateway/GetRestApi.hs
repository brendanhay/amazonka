{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the 'RestApi' resource in the collection.
module Network.AWS.ApiGateway.GetRestApi
    (
    -- * Creating a request
      GetRestApi (..)
    , mkGetRestApi
    -- ** Request lenses
    , graRestApiId

     -- * Destructuring the response
    , Types.RestApi (..)
    , Types.mkRestApi
    -- ** Response lenses
    , Types.raApiKeySource
    , Types.raBinaryMediaTypes
    , Types.raCreatedDate
    , Types.raDescription
    , Types.raDisableExecuteApiEndpoint
    , Types.raEndpointConfiguration
    , Types.raId
    , Types.raMinimumCompressionSize
    , Types.raName
    , Types.raPolicy
    , Types.raTags
    , Types.raVersion
    , Types.raWarnings
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to list an existing 'RestApi' defined for your collection. 
--
-- /See:/ 'mkGetRestApi' smart constructor.
newtype GetRestApi = GetRestApi'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRestApi' value with any optional fields omitted.
mkGetRestApi
    :: Core.Text -- ^ 'restApiId'
    -> GetRestApi
mkGetRestApi restApiId = GetRestApi'{restApiId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graRestApiId :: Lens.Lens' GetRestApi Core.Text
graRestApiId = Lens.field @"restApiId"
{-# INLINEABLE graRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

instance Core.ToQuery GetRestApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRestApi where
        toHeaders GetRestApi{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetRestApi where
        type Rs GetRestApi = Types.RestApi
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/restapis/" Core.<> Core.toText restApiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
