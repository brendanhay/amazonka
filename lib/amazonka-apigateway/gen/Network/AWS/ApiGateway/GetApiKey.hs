{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ApiKey' resource.
module Network.AWS.ApiGateway.GetApiKey
    (
    -- * Creating a request
      GetApiKey (..)
    , mkGetApiKey
    -- ** Request lenses
    , gakApiKey
    , gakIncludeValue

     -- * Destructuring the response
    , Types.ApiKey (..)
    , Types.mkApiKey
    -- ** Response lenses
    , Types.akCreatedDate
    , Types.akCustomerId
    , Types.akDescription
    , Types.akEnabled
    , Types.akId
    , Types.akLastUpdatedDate
    , Types.akName
    , Types.akStageKeys
    , Types.akTags
    , Types.akValue
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current 'ApiKey' resource.
--
-- /See:/ 'mkGetApiKey' smart constructor.
data GetApiKey = GetApiKey'
  { apiKey :: Core.Text
    -- ^ [Required] The identifier of the 'ApiKey' resource.
  , includeValue :: Core.Maybe Core.Bool
    -- ^ A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApiKey' value with any optional fields omitted.
mkGetApiKey
    :: Core.Text -- ^ 'apiKey'
    -> GetApiKey
mkGetApiKey apiKey
  = GetApiKey'{apiKey, includeValue = Core.Nothing}

-- | [Required] The identifier of the 'ApiKey' resource.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakApiKey :: Lens.Lens' GetApiKey Core.Text
gakApiKey = Lens.field @"apiKey"
{-# INLINEABLE gakApiKey #-}
{-# DEPRECATED apiKey "Use generic-lens or generic-optics with 'apiKey' instead"  #-}

-- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
--
-- /Note:/ Consider using 'includeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakIncludeValue :: Lens.Lens' GetApiKey (Core.Maybe Core.Bool)
gakIncludeValue = Lens.field @"includeValue"
{-# INLINEABLE gakIncludeValue #-}
{-# DEPRECATED includeValue "Use generic-lens or generic-optics with 'includeValue' instead"  #-}

instance Core.ToQuery GetApiKey where
        toQuery GetApiKey{..}
          = Core.maybe Core.mempty (Core.toQueryPair "includeValue")
              includeValue

instance Core.ToHeaders GetApiKey where
        toHeaders GetApiKey{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetApiKey where
        type Rs GetApiKey = Types.ApiKey
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/apikeys/" Core.<> Core.toText apiKey,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
