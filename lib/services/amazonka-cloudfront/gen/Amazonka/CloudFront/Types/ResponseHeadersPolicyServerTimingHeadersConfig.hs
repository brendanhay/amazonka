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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyServerTimingHeadersConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyServerTimingHeadersConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration for enabling the @Server-Timing@ header in HTTP
-- responses sent from CloudFront. CloudFront adds this header to HTTP
-- responses that it sends in response to requests that match a cache
-- behavior that\'s associated with this response headers policy.
--
-- You can use the @Server-Timing@ header to view metrics that can help you
-- gain insights about the behavior and performance of CloudFront. For
-- example, you can see which cache layer served a cache hit, or the first
-- byte latency from the origin when there was a cache miss. You can use
-- the metrics in the @Server-Timing@ header to troubleshoot issues or test
-- the efficiency of your CloudFront configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/understanding-response-headers-policies.html#server-timing-header Server-Timing header>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newResponseHeadersPolicyServerTimingHeadersConfig' smart constructor.
data ResponseHeadersPolicyServerTimingHeadersConfig = ResponseHeadersPolicyServerTimingHeadersConfig'
  { -- | A number 0–100 (inclusive) that specifies the percentage of responses
    -- that you want CloudFront to add the @Server-Timing@ header to. When you
    -- set the sampling rate to 100, CloudFront adds the @Server-Timing@ header
    -- to the HTTP response for every request that matches the cache behavior
    -- that this response headers policy is attached to. When you set it to 50,
    -- CloudFront adds the header to 50% of the responses for requests that
    -- match the cache behavior. You can set the sampling rate to any number
    -- 0–100 with up to four decimal places.
    samplingRate :: Prelude.Maybe Prelude.Double,
    -- | A Boolean that determines whether CloudFront adds the @Server-Timing@
    -- header to HTTP responses that it sends in response to requests that
    -- match a cache behavior that\'s associated with this response headers
    -- policy.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyServerTimingHeadersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRate', 'responseHeadersPolicyServerTimingHeadersConfig_samplingRate' - A number 0–100 (inclusive) that specifies the percentage of responses
-- that you want CloudFront to add the @Server-Timing@ header to. When you
-- set the sampling rate to 100, CloudFront adds the @Server-Timing@ header
-- to the HTTP response for every request that matches the cache behavior
-- that this response headers policy is attached to. When you set it to 50,
-- CloudFront adds the header to 50% of the responses for requests that
-- match the cache behavior. You can set the sampling rate to any number
-- 0–100 with up to four decimal places.
--
-- 'enabled', 'responseHeadersPolicyServerTimingHeadersConfig_enabled' - A Boolean that determines whether CloudFront adds the @Server-Timing@
-- header to HTTP responses that it sends in response to requests that
-- match a cache behavior that\'s associated with this response headers
-- policy.
newResponseHeadersPolicyServerTimingHeadersConfig ::
  -- | 'enabled'
  Prelude.Bool ->
  ResponseHeadersPolicyServerTimingHeadersConfig
newResponseHeadersPolicyServerTimingHeadersConfig
  pEnabled_ =
    ResponseHeadersPolicyServerTimingHeadersConfig'
      { samplingRate =
          Prelude.Nothing,
        enabled = pEnabled_
      }

-- | A number 0–100 (inclusive) that specifies the percentage of responses
-- that you want CloudFront to add the @Server-Timing@ header to. When you
-- set the sampling rate to 100, CloudFront adds the @Server-Timing@ header
-- to the HTTP response for every request that matches the cache behavior
-- that this response headers policy is attached to. When you set it to 50,
-- CloudFront adds the header to 50% of the responses for requests that
-- match the cache behavior. You can set the sampling rate to any number
-- 0–100 with up to four decimal places.
responseHeadersPolicyServerTimingHeadersConfig_samplingRate :: Lens.Lens' ResponseHeadersPolicyServerTimingHeadersConfig (Prelude.Maybe Prelude.Double)
responseHeadersPolicyServerTimingHeadersConfig_samplingRate = Lens.lens (\ResponseHeadersPolicyServerTimingHeadersConfig' {samplingRate} -> samplingRate) (\s@ResponseHeadersPolicyServerTimingHeadersConfig' {} a -> s {samplingRate = a} :: ResponseHeadersPolicyServerTimingHeadersConfig)

-- | A Boolean that determines whether CloudFront adds the @Server-Timing@
-- header to HTTP responses that it sends in response to requests that
-- match a cache behavior that\'s associated with this response headers
-- policy.
responseHeadersPolicyServerTimingHeadersConfig_enabled :: Lens.Lens' ResponseHeadersPolicyServerTimingHeadersConfig Prelude.Bool
responseHeadersPolicyServerTimingHeadersConfig_enabled = Lens.lens (\ResponseHeadersPolicyServerTimingHeadersConfig' {enabled} -> enabled) (\s@ResponseHeadersPolicyServerTimingHeadersConfig' {} a -> s {enabled = a} :: ResponseHeadersPolicyServerTimingHeadersConfig)

instance
  Data.FromXML
    ResponseHeadersPolicyServerTimingHeadersConfig
  where
  parseXML x =
    ResponseHeadersPolicyServerTimingHeadersConfig'
      Prelude.<$> (x Data..@? "SamplingRate")
      Prelude.<*> (x Data..@ "Enabled")

instance
  Prelude.Hashable
    ResponseHeadersPolicyServerTimingHeadersConfig
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyServerTimingHeadersConfig' {..} =
      _salt
        `Prelude.hashWithSalt` samplingRate
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    ResponseHeadersPolicyServerTimingHeadersConfig
  where
  rnf
    ResponseHeadersPolicyServerTimingHeadersConfig' {..} =
      Prelude.rnf samplingRate
        `Prelude.seq` Prelude.rnf enabled

instance
  Data.ToXML
    ResponseHeadersPolicyServerTimingHeadersConfig
  where
  toXML
    ResponseHeadersPolicyServerTimingHeadersConfig' {..} =
      Prelude.mconcat
        [ "SamplingRate" Data.@= samplingRate,
          "Enabled" Data.@= enabled
        ]
