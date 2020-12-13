{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointRequestConfiguration
  ( HTTPEndpointRequestConfiguration (..),

    -- * Smart constructor
    mkHTTPEndpointRequestConfiguration,

    -- * Lenses
    httpercCommonAttributes,
    httpercContentEncoding,
  )
where

import Network.AWS.Firehose.Types.ContentEncoding
import Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration of the HTTP endpoint request.
--
-- /See:/ 'mkHTTPEndpointRequestConfiguration' smart constructor.
data HTTPEndpointRequestConfiguration = HTTPEndpointRequestConfiguration'
  { -- | Describes the metadata sent to the HTTP endpoint destination.
    commonAttributes :: Lude.Maybe [HTTPEndpointCommonAttribute],
    -- | Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
    contentEncoding :: Lude.Maybe ContentEncoding
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointRequestConfiguration' with the minimum fields required to make a request.
--
-- * 'commonAttributes' - Describes the metadata sent to the HTTP endpoint destination.
-- * 'contentEncoding' - Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
mkHTTPEndpointRequestConfiguration ::
  HTTPEndpointRequestConfiguration
mkHTTPEndpointRequestConfiguration =
  HTTPEndpointRequestConfiguration'
    { commonAttributes =
        Lude.Nothing,
      contentEncoding = Lude.Nothing
    }

-- | Describes the metadata sent to the HTTP endpoint destination.
--
-- /Note:/ Consider using 'commonAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpercCommonAttributes :: Lens.Lens' HTTPEndpointRequestConfiguration (Lude.Maybe [HTTPEndpointCommonAttribute])
httpercCommonAttributes = Lens.lens (commonAttributes :: HTTPEndpointRequestConfiguration -> Lude.Maybe [HTTPEndpointCommonAttribute]) (\s a -> s {commonAttributes = a} :: HTTPEndpointRequestConfiguration)
{-# DEPRECATED httpercCommonAttributes "Use generic-lens or generic-optics with 'commonAttributes' instead." #-}

-- | Kinesis Data Firehose uses the content encoding to compress the body of a request before sending the request to the destination. For more information, see <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding> in MDN Web Docs, the official Mozilla documentation.
--
-- /Note:/ Consider using 'contentEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpercContentEncoding :: Lens.Lens' HTTPEndpointRequestConfiguration (Lude.Maybe ContentEncoding)
httpercContentEncoding = Lens.lens (contentEncoding :: HTTPEndpointRequestConfiguration -> Lude.Maybe ContentEncoding) (\s a -> s {contentEncoding = a} :: HTTPEndpointRequestConfiguration)
{-# DEPRECATED httpercContentEncoding "Use generic-lens or generic-optics with 'contentEncoding' instead." #-}

instance Lude.FromJSON HTTPEndpointRequestConfiguration where
  parseJSON =
    Lude.withObject
      "HTTPEndpointRequestConfiguration"
      ( \x ->
          HTTPEndpointRequestConfiguration'
            Lude.<$> (x Lude..:? "CommonAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ContentEncoding")
      )

instance Lude.ToJSON HTTPEndpointRequestConfiguration where
  toJSON HTTPEndpointRequestConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CommonAttributes" Lude..=) Lude.<$> commonAttributes,
            ("ContentEncoding" Lude..=) Lude.<$> contentEncoding
          ]
      )
