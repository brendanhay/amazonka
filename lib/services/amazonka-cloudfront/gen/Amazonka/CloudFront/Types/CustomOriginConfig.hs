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
-- Module      : Amazonka.CloudFront.Types.CustomOriginConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CustomOriginConfig where

import Amazonka.CloudFront.Types.OriginProtocolPolicy
import Amazonka.CloudFront.Types.OriginSslProtocols
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom origin. A custom origin is any origin that is /not/ an Amazon
-- S3 bucket, with one exception. An Amazon S3 bucket that is
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html configured with static website hosting>
-- /is/ a custom origin.
--
-- /See:/ 'newCustomOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
  { -- | Specifies how long, in seconds, CloudFront persists its connection to
    -- the origin. The minimum timeout is 1 second, the maximum is 60 seconds,
    -- and the default (if you don\'t specify otherwise) is 5 seconds.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout>
    -- in the /Amazon CloudFront Developer Guide/.
    originKeepaliveTimeout :: Prelude.Maybe Prelude.Int,
    -- | Specifies how long, in seconds, CloudFront waits for a response from the
    -- origin. This is also known as the /origin response timeout/. The minimum
    -- timeout is 1 second, the maximum is 60 seconds, and the default (if you
    -- don\'t specify otherwise) is 30 seconds.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>
    -- in the /Amazon CloudFront Developer Guide/.
    originReadTimeout :: Prelude.Maybe Prelude.Int,
    -- | Specifies the minimum SSL\/TLS protocol that CloudFront uses when
    -- connecting to your origin over HTTPS. Valid values include @SSLv3@,
    -- @TLSv1@, @TLSv1.1@, and @TLSv1.2@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol>
    -- in the /Amazon CloudFront Developer Guide/.
    originSslProtocols :: Prelude.Maybe OriginSslProtocols,
    -- | The HTTP port that CloudFront uses to connect to the origin. Specify the
    -- HTTP port that the origin listens on.
    hTTPPort :: Prelude.Int,
    -- | The HTTPS port that CloudFront uses to connect to the origin. Specify
    -- the HTTPS port that the origin listens on.
    hTTPSPort :: Prelude.Int,
    -- | Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect
    -- to the origin. Valid values are:
    --
    -- -   @http-only@ – CloudFront always uses HTTP to connect to the origin.
    --
    -- -   @match-viewer@ – CloudFront connects to the origin using the same
    --     protocol that the viewer used to connect to CloudFront.
    --
    -- -   @https-only@ – CloudFront always uses HTTPS to connect to the
    --     origin.
    originProtocolPolicy :: OriginProtocolPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomOriginConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originKeepaliveTimeout', 'customOriginConfig_originKeepaliveTimeout' - Specifies how long, in seconds, CloudFront persists its connection to
-- the origin. The minimum timeout is 1 second, the maximum is 60 seconds,
-- and the default (if you don\'t specify otherwise) is 5 seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'originReadTimeout', 'customOriginConfig_originReadTimeout' - Specifies how long, in seconds, CloudFront waits for a response from the
-- origin. This is also known as the /origin response timeout/. The minimum
-- timeout is 1 second, the maximum is 60 seconds, and the default (if you
-- don\'t specify otherwise) is 30 seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'originSslProtocols', 'customOriginConfig_originSslProtocols' - Specifies the minimum SSL\/TLS protocol that CloudFront uses when
-- connecting to your origin over HTTPS. Valid values include @SSLv3@,
-- @TLSv1@, @TLSv1.1@, and @TLSv1.2@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'hTTPPort', 'customOriginConfig_hTTPPort' - The HTTP port that CloudFront uses to connect to the origin. Specify the
-- HTTP port that the origin listens on.
--
-- 'hTTPSPort', 'customOriginConfig_hTTPSPort' - The HTTPS port that CloudFront uses to connect to the origin. Specify
-- the HTTPS port that the origin listens on.
--
-- 'originProtocolPolicy', 'customOriginConfig_originProtocolPolicy' - Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect
-- to the origin. Valid values are:
--
-- -   @http-only@ – CloudFront always uses HTTP to connect to the origin.
--
-- -   @match-viewer@ – CloudFront connects to the origin using the same
--     protocol that the viewer used to connect to CloudFront.
--
-- -   @https-only@ – CloudFront always uses HTTPS to connect to the
--     origin.
newCustomOriginConfig ::
  -- | 'hTTPPort'
  Prelude.Int ->
  -- | 'hTTPSPort'
  Prelude.Int ->
  -- | 'originProtocolPolicy'
  OriginProtocolPolicy ->
  CustomOriginConfig
newCustomOriginConfig
  pHTTPPort_
  pHTTPSPort_
  pOriginProtocolPolicy_ =
    CustomOriginConfig'
      { originKeepaliveTimeout =
          Prelude.Nothing,
        originReadTimeout = Prelude.Nothing,
        originSslProtocols = Prelude.Nothing,
        hTTPPort = pHTTPPort_,
        hTTPSPort = pHTTPSPort_,
        originProtocolPolicy = pOriginProtocolPolicy_
      }

-- | Specifies how long, in seconds, CloudFront persists its connection to
-- the origin. The minimum timeout is 1 second, the maximum is 60 seconds,
-- and the default (if you don\'t specify otherwise) is 5 seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout>
-- in the /Amazon CloudFront Developer Guide/.
customOriginConfig_originKeepaliveTimeout :: Lens.Lens' CustomOriginConfig (Prelude.Maybe Prelude.Int)
customOriginConfig_originKeepaliveTimeout = Lens.lens (\CustomOriginConfig' {originKeepaliveTimeout} -> originKeepaliveTimeout) (\s@CustomOriginConfig' {} a -> s {originKeepaliveTimeout = a} :: CustomOriginConfig)

-- | Specifies how long, in seconds, CloudFront waits for a response from the
-- origin. This is also known as the /origin response timeout/. The minimum
-- timeout is 1 second, the maximum is 60 seconds, and the default (if you
-- don\'t specify otherwise) is 30 seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>
-- in the /Amazon CloudFront Developer Guide/.
customOriginConfig_originReadTimeout :: Lens.Lens' CustomOriginConfig (Prelude.Maybe Prelude.Int)
customOriginConfig_originReadTimeout = Lens.lens (\CustomOriginConfig' {originReadTimeout} -> originReadTimeout) (\s@CustomOriginConfig' {} a -> s {originReadTimeout = a} :: CustomOriginConfig)

-- | Specifies the minimum SSL\/TLS protocol that CloudFront uses when
-- connecting to your origin over HTTPS. Valid values include @SSLv3@,
-- @TLSv1@, @TLSv1.1@, and @TLSv1.2@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol>
-- in the /Amazon CloudFront Developer Guide/.
customOriginConfig_originSslProtocols :: Lens.Lens' CustomOriginConfig (Prelude.Maybe OriginSslProtocols)
customOriginConfig_originSslProtocols = Lens.lens (\CustomOriginConfig' {originSslProtocols} -> originSslProtocols) (\s@CustomOriginConfig' {} a -> s {originSslProtocols = a} :: CustomOriginConfig)

-- | The HTTP port that CloudFront uses to connect to the origin. Specify the
-- HTTP port that the origin listens on.
customOriginConfig_hTTPPort :: Lens.Lens' CustomOriginConfig Prelude.Int
customOriginConfig_hTTPPort = Lens.lens (\CustomOriginConfig' {hTTPPort} -> hTTPPort) (\s@CustomOriginConfig' {} a -> s {hTTPPort = a} :: CustomOriginConfig)

-- | The HTTPS port that CloudFront uses to connect to the origin. Specify
-- the HTTPS port that the origin listens on.
customOriginConfig_hTTPSPort :: Lens.Lens' CustomOriginConfig Prelude.Int
customOriginConfig_hTTPSPort = Lens.lens (\CustomOriginConfig' {hTTPSPort} -> hTTPSPort) (\s@CustomOriginConfig' {} a -> s {hTTPSPort = a} :: CustomOriginConfig)

-- | Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect
-- to the origin. Valid values are:
--
-- -   @http-only@ – CloudFront always uses HTTP to connect to the origin.
--
-- -   @match-viewer@ – CloudFront connects to the origin using the same
--     protocol that the viewer used to connect to CloudFront.
--
-- -   @https-only@ – CloudFront always uses HTTPS to connect to the
--     origin.
customOriginConfig_originProtocolPolicy :: Lens.Lens' CustomOriginConfig OriginProtocolPolicy
customOriginConfig_originProtocolPolicy = Lens.lens (\CustomOriginConfig' {originProtocolPolicy} -> originProtocolPolicy) (\s@CustomOriginConfig' {} a -> s {originProtocolPolicy = a} :: CustomOriginConfig)

instance Data.FromXML CustomOriginConfig where
  parseXML x =
    CustomOriginConfig'
      Prelude.<$> (x Data..@? "OriginKeepaliveTimeout")
      Prelude.<*> (x Data..@? "OriginReadTimeout")
      Prelude.<*> (x Data..@? "OriginSslProtocols")
      Prelude.<*> (x Data..@ "HTTPPort")
      Prelude.<*> (x Data..@ "HTTPSPort")
      Prelude.<*> (x Data..@ "OriginProtocolPolicy")

instance Prelude.Hashable CustomOriginConfig where
  hashWithSalt _salt CustomOriginConfig' {..} =
    _salt
      `Prelude.hashWithSalt` originKeepaliveTimeout
      `Prelude.hashWithSalt` originReadTimeout
      `Prelude.hashWithSalt` originSslProtocols
      `Prelude.hashWithSalt` hTTPPort
      `Prelude.hashWithSalt` hTTPSPort
      `Prelude.hashWithSalt` originProtocolPolicy

instance Prelude.NFData CustomOriginConfig where
  rnf CustomOriginConfig' {..} =
    Prelude.rnf originKeepaliveTimeout
      `Prelude.seq` Prelude.rnf originReadTimeout
      `Prelude.seq` Prelude.rnf originSslProtocols
      `Prelude.seq` Prelude.rnf hTTPPort
      `Prelude.seq` Prelude.rnf hTTPSPort
      `Prelude.seq` Prelude.rnf originProtocolPolicy

instance Data.ToXML CustomOriginConfig where
  toXML CustomOriginConfig' {..} =
    Prelude.mconcat
      [ "OriginKeepaliveTimeout"
          Data.@= originKeepaliveTimeout,
        "OriginReadTimeout" Data.@= originReadTimeout,
        "OriginSslProtocols" Data.@= originSslProtocols,
        "HTTPPort" Data.@= hTTPPort,
        "HTTPSPort" Data.@= hTTPSPort,
        "OriginProtocolPolicy" Data.@= originProtocolPolicy
      ]
