{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
  ( HttpEndpointRequestConfiguration (..)
  -- * Smart constructor
  , mkHttpEndpointRequestConfiguration
  -- * Lenses
  , hercCommonAttributes
  , hercContentEncoding
  ) where

import qualified Network.AWS.Firehose.Types.ContentEncoding as Types
import qualified Network.AWS.Firehose.Types.HttpEndpointCommonAttribute as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration of the HTTP endpoint request.
--
-- /See:/ 'mkHttpEndpointRequestConfiguration' smart constructor.
data HttpEndpointRequestConfiguration = HttpEndpointRequestConfiguration'
  { commonAttributes :: Core.Maybe [Types.HttpEndpointCommonAttribute]
    -- ^ Describes the metadata sent to the HTTP endpoint destination.
  , contentEncoding :: Core.Maybe Types.ContentEncoding
    -- ^ Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpEndpointRequestConfiguration' value with any optional fields omitted.
mkHttpEndpointRequestConfiguration
    :: HttpEndpointRequestConfiguration
mkHttpEndpointRequestConfiguration
  = HttpEndpointRequestConfiguration'{commonAttributes =
                                        Core.Nothing,
                                      contentEncoding = Core.Nothing}

-- | Describes the metadata sent to the HTTP endpoint destination.
--
-- /Note:/ Consider using 'commonAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hercCommonAttributes :: Lens.Lens' HttpEndpointRequestConfiguration (Core.Maybe [Types.HttpEndpointCommonAttribute])
hercCommonAttributes = Lens.field @"commonAttributes"
{-# INLINEABLE hercCommonAttributes #-}
{-# DEPRECATED commonAttributes "Use generic-lens or generic-optics with 'commonAttributes' instead"  #-}

-- | Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hercContentEncoding :: Lens.Lens' HttpEndpointRequestConfiguration (Core.Maybe Types.ContentEncoding)
hercContentEncoding = Lens.field @"contentEncoding"
{-# INLINEABLE hercContentEncoding #-}
{-# DEPRECATED contentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead"  #-}

instance Core.FromJSON HttpEndpointRequestConfiguration where
        toJSON HttpEndpointRequestConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("CommonAttributes" Core..=) Core.<$> commonAttributes,
                  ("ContentEncoding" Core..=) Core.<$> contentEncoding])

instance Core.FromJSON HttpEndpointRequestConfiguration where
        parseJSON
          = Core.withObject "HttpEndpointRequestConfiguration" Core.$
              \ x ->
                HttpEndpointRequestConfiguration' Core.<$>
                  (x Core..:? "CommonAttributes") Core.<*>
                    x Core..:? "ContentEncoding"
