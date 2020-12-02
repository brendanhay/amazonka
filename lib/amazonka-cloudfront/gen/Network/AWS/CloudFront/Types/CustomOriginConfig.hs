{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomOriginConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomOriginConfig where

import Network.AWS.CloudFront.Types.OriginProtocolPolicy
import Network.AWS.CloudFront.Types.OriginSSLProtocols
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom origin. A custom origin is any origin that is /not/ an Amazon S3 bucket, with one exception. An Amazon S3 bucket that is <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html configured with static website hosting> /is/ a custom origin.
--
--
--
-- /See:/ 'customOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
  { _cocOriginKeepaliveTimeout ::
      !(Maybe Int),
    _cocOriginReadTimeout :: !(Maybe Int),
    _cocOriginSSLProtocols :: !(Maybe OriginSSLProtocols),
    _cocHTTPPort :: !Int,
    _cocHTTPSPort :: !Int,
    _cocOriginProtocolPolicy :: !OriginProtocolPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomOriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cocOriginKeepaliveTimeout' - Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cocOriginReadTimeout' - Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cocOriginSSLProtocols' - Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cocHTTPPort' - The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
--
-- * 'cocHTTPSPort' - The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
--
-- * 'cocOriginProtocolPolicy' - Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect to the origin. Valid values are:     * @http-only@ – CloudFront always uses HTTP to connect to the origin.     * @match-viewer@ – CloudFront connects to the origin using the same protocol that the viewer used to connect to CloudFront.     * @https-only@ – CloudFront always uses HTTPS to connect to the origin.
customOriginConfig ::
  -- | 'cocHTTPPort'
  Int ->
  -- | 'cocHTTPSPort'
  Int ->
  -- | 'cocOriginProtocolPolicy'
  OriginProtocolPolicy ->
  CustomOriginConfig
customOriginConfig pHTTPPort_ pHTTPSPort_ pOriginProtocolPolicy_ =
  CustomOriginConfig'
    { _cocOriginKeepaliveTimeout = Nothing,
      _cocOriginReadTimeout = Nothing,
      _cocOriginSSLProtocols = Nothing,
      _cocHTTPPort = pHTTPPort_,
      _cocHTTPSPort = pHTTPSPort_,
      _cocOriginProtocolPolicy = pOriginProtocolPolicy_
    }

-- | Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
cocOriginKeepaliveTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginKeepaliveTimeout = lens _cocOriginKeepaliveTimeout (\s a -> s {_cocOriginKeepaliveTimeout = a})

-- | Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
cocOriginReadTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginReadTimeout = lens _cocOriginReadTimeout (\s a -> s {_cocOriginReadTimeout = a})

-- | Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
cocOriginSSLProtocols :: Lens' CustomOriginConfig (Maybe OriginSSLProtocols)
cocOriginSSLProtocols = lens _cocOriginSSLProtocols (\s a -> s {_cocOriginSSLProtocols = a})

-- | The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\s a -> s {_cocHTTPPort = a})

-- | The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\s a -> s {_cocHTTPSPort = a})

-- | Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect to the origin. Valid values are:     * @http-only@ – CloudFront always uses HTTP to connect to the origin.     * @match-viewer@ – CloudFront connects to the origin using the same protocol that the viewer used to connect to CloudFront.     * @https-only@ – CloudFront always uses HTTPS to connect to the origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\s a -> s {_cocOriginProtocolPolicy = a})

instance FromXML CustomOriginConfig where
  parseXML x =
    CustomOriginConfig'
      <$> (x .@? "OriginKeepaliveTimeout")
      <*> (x .@? "OriginReadTimeout")
      <*> (x .@? "OriginSslProtocols")
      <*> (x .@ "HTTPPort")
      <*> (x .@ "HTTPSPort")
      <*> (x .@ "OriginProtocolPolicy")

instance Hashable CustomOriginConfig

instance NFData CustomOriginConfig

instance ToXML CustomOriginConfig where
  toXML CustomOriginConfig' {..} =
    mconcat
      [ "OriginKeepaliveTimeout" @= _cocOriginKeepaliveTimeout,
        "OriginReadTimeout" @= _cocOriginReadTimeout,
        "OriginSslProtocols" @= _cocOriginSSLProtocols,
        "HTTPPort" @= _cocHTTPPort,
        "HTTPSPort" @= _cocHTTPSPort,
        "OriginProtocolPolicy" @= _cocOriginProtocolPolicy
      ]
