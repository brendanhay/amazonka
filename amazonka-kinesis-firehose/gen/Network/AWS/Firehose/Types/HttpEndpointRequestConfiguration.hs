{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types.ContentEncoding
import Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
import qualified Network.AWS.Lens as Lens

-- | The configuration of the HTTP endpoint request.
--
-- /See:/ 'newHttpEndpointRequestConfiguration' smart constructor.
data HttpEndpointRequestConfiguration = HttpEndpointRequestConfiguration'
  { -- | Kinesis Data Firehose uses the content encoding to compress the body of
    -- a request before sending the request to the destination. For more
    -- information, see
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding>
    -- in MDN Web Docs, the official Mozilla documentation.
    contentEncoding :: Core.Maybe ContentEncoding,
    -- | Describes the metadata sent to the HTTP endpoint destination.
    commonAttributes :: Core.Maybe [HttpEndpointCommonAttribute]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'HttpEndpointRequestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentEncoding', 'httpEndpointRequestConfiguration_contentEncoding' - Kinesis Data Firehose uses the content encoding to compress the body of
-- a request before sending the request to the destination. For more
-- information, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding>
-- in MDN Web Docs, the official Mozilla documentation.
--
-- 'commonAttributes', 'httpEndpointRequestConfiguration_commonAttributes' - Describes the metadata sent to the HTTP endpoint destination.
newHttpEndpointRequestConfiguration ::
  HttpEndpointRequestConfiguration
newHttpEndpointRequestConfiguration =
  HttpEndpointRequestConfiguration'
    { contentEncoding =
        Core.Nothing,
      commonAttributes = Core.Nothing
    }

-- | Kinesis Data Firehose uses the content encoding to compress the body of
-- a request before sending the request to the destination. For more
-- information, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding Content-Encoding>
-- in MDN Web Docs, the official Mozilla documentation.
httpEndpointRequestConfiguration_contentEncoding :: Lens.Lens' HttpEndpointRequestConfiguration (Core.Maybe ContentEncoding)
httpEndpointRequestConfiguration_contentEncoding = Lens.lens (\HttpEndpointRequestConfiguration' {contentEncoding} -> contentEncoding) (\s@HttpEndpointRequestConfiguration' {} a -> s {contentEncoding = a} :: HttpEndpointRequestConfiguration)

-- | Describes the metadata sent to the HTTP endpoint destination.
httpEndpointRequestConfiguration_commonAttributes :: Lens.Lens' HttpEndpointRequestConfiguration (Core.Maybe [HttpEndpointCommonAttribute])
httpEndpointRequestConfiguration_commonAttributes = Lens.lens (\HttpEndpointRequestConfiguration' {commonAttributes} -> commonAttributes) (\s@HttpEndpointRequestConfiguration' {} a -> s {commonAttributes = a} :: HttpEndpointRequestConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    HttpEndpointRequestConfiguration
  where
  parseJSON =
    Core.withObject
      "HttpEndpointRequestConfiguration"
      ( \x ->
          HttpEndpointRequestConfiguration'
            Core.<$> (x Core..:? "ContentEncoding")
            Core.<*> (x Core..:? "CommonAttributes" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    HttpEndpointRequestConfiguration

instance Core.NFData HttpEndpointRequestConfiguration

instance Core.ToJSON HttpEndpointRequestConfiguration where
  toJSON HttpEndpointRequestConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContentEncoding" Core..=)
              Core.<$> contentEncoding,
            ("CommonAttributes" Core..=)
              Core.<$> commonAttributes
          ]
      )
