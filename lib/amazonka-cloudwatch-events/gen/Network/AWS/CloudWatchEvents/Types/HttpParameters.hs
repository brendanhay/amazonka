{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.HttpParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.HttpParameters
  ( HttpParameters (..)
  -- * Smart constructor
  , mkHttpParameters
  -- * Lenses
  , hpHeaderParameters
  , hpPathParameterValues
  , hpQueryStringParameters
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.HeaderKey as Types
import qualified Network.AWS.CloudWatchEvents.Types.HeaderValue as Types
import qualified Network.AWS.CloudWatchEvents.Types.PathParameter as Types
import qualified Network.AWS.CloudWatchEvents.Types.QueryStringKey as Types
import qualified Network.AWS.CloudWatchEvents.Types.QueryStringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | These are custom parameter to be used when the target is an API Gateway REST APIs.
--
-- /See:/ 'mkHttpParameters' smart constructor.
data HttpParameters = HttpParameters'
  { headerParameters :: Core.Maybe (Core.HashMap Types.HeaderKey Types.HeaderValue)
    -- ^ The headers that need to be sent as part of request invoking the API Gateway REST API.
  , pathParameterValues :: Core.Maybe [Types.PathParameter]
    -- ^ The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
  , queryStringParameters :: Core.Maybe (Core.HashMap Types.QueryStringKey Types.QueryStringValue)
    -- ^ The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpParameters' value with any optional fields omitted.
mkHttpParameters
    :: HttpParameters
mkHttpParameters
  = HttpParameters'{headerParameters = Core.Nothing,
                    pathParameterValues = Core.Nothing,
                    queryStringParameters = Core.Nothing}

-- | The headers that need to be sent as part of request invoking the API Gateway REST API.
--
-- /Note:/ Consider using 'headerParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpHeaderParameters :: Lens.Lens' HttpParameters (Core.Maybe (Core.HashMap Types.HeaderKey Types.HeaderValue))
hpHeaderParameters = Lens.field @"headerParameters"
{-# INLINEABLE hpHeaderParameters #-}
{-# DEPRECATED headerParameters "Use generic-lens or generic-optics with 'headerParameters' instead"  #-}

-- | The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
--
-- /Note:/ Consider using 'pathParameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpPathParameterValues :: Lens.Lens' HttpParameters (Core.Maybe [Types.PathParameter])
hpPathParameterValues = Lens.field @"pathParameterValues"
{-# INLINEABLE hpPathParameterValues #-}
{-# DEPRECATED pathParameterValues "Use generic-lens or generic-optics with 'pathParameterValues' instead"  #-}

-- | The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
--
-- /Note:/ Consider using 'queryStringParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpQueryStringParameters :: Lens.Lens' HttpParameters (Core.Maybe (Core.HashMap Types.QueryStringKey Types.QueryStringValue))
hpQueryStringParameters = Lens.field @"queryStringParameters"
{-# INLINEABLE hpQueryStringParameters #-}
{-# DEPRECATED queryStringParameters "Use generic-lens or generic-optics with 'queryStringParameters' instead"  #-}

instance Core.FromJSON HttpParameters where
        toJSON HttpParameters{..}
          = Core.object
              (Core.catMaybes
                 [("HeaderParameters" Core..=) Core.<$> headerParameters,
                  ("PathParameterValues" Core..=) Core.<$> pathParameterValues,
                  ("QueryStringParameters" Core..=) Core.<$> queryStringParameters])

instance Core.FromJSON HttpParameters where
        parseJSON
          = Core.withObject "HttpParameters" Core.$
              \ x ->
                HttpParameters' Core.<$>
                  (x Core..:? "HeaderParameters") Core.<*>
                    x Core..:? "PathParameterValues"
                    Core.<*> x Core..:? "QueryStringParameters"
