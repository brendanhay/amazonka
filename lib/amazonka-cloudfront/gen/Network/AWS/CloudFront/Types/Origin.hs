{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origin where

import Network.AWS.CloudFront.Types.CustomHeaders
import Network.AWS.CloudFront.Types.CustomOriginConfig
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.S3OriginConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An origin.
--
--
-- An origin is the location where content is stored, and from which CloudFront gets content to serve to viewers. To specify an origin:
--
--     * Use @S3OriginConfig@ to specify an Amazon S3 bucket that is not configured with static website hosting.
--
--     * Use @CustomOriginConfig@ to specify all other kinds of origins, including:
--
--     * An Amazon S3 bucket that is configured with static website hosting
--
--     * An Elastic Load Balancing load balancer
--
--     * An AWS Elemental MediaPackage endpoint
--
--     * An AWS Elemental MediaStore container
--
--     * Any other HTTP server, running on an Amazon EC2 instance or any other kind of host
--
--
--
--
--
-- For the current maximum number of origins that you can specify per distribution, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html#limits-web-distributions General Quotas on Web Distributions> in the /Amazon CloudFront Developer Guide/ (quotas were formerly referred to as limits).
--
--
-- /See:/ 'origin' smart constructor.
data Origin = Origin'
  { _oCustomHeaders :: !(Maybe CustomHeaders),
    _oCustomOriginConfig :: !(Maybe CustomOriginConfig),
    _oConnectionTimeout :: !(Maybe Int),
    _oConnectionAttempts :: !(Maybe Int),
    _oS3OriginConfig :: !(Maybe S3OriginConfig),
    _oOriginPath :: !(Maybe Text),
    _oOriginShield :: !(Maybe OriginShield),
    _oId :: !Text,
    _oDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCustomHeaders' - A list of HTTP header names and values that CloudFront adds to the requests that it sends to the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oCustomOriginConfig' - Use this type to specify an origin that is not an Amazon S3 bucket, with one exception. If the Amazon S3 bucket is configured with static website hosting, use this type. If the Amazon S3 bucket is not configured with static website hosting, use the @S3OriginConfig@ type instead.
--
-- * 'oConnectionTimeout' - The number of seconds that CloudFront waits when trying to establish a connection to the origin. The minimum timeout is 1 second, the maximum is 10 seconds, and the default (if you don’t specify otherwise) is 10 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oConnectionAttempts' - The number of times that CloudFront attempts to connect to the origin. The minimum number is 1, the maximum is 3, and the default (if you don’t specify otherwise) is 3. For a custom origin (including an Amazon S3 bucket that’s configured with static website hosting), this value also specifies the number of times that CloudFront attempts to get a response from the origin, in the case of an <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oS3OriginConfig' - Use this type to specify an origin that is an Amazon S3 bucket that is not configured with static website hosting. To specify any other type of origin, including an Amazon S3 bucket that is configured with static website hosting, use the @CustomOriginConfig@ type instead.
--
-- * 'oOriginPath' - An optional path that CloudFront appends to the origin domain name when CloudFront requests content from the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oOriginShield' - CloudFront Origin Shield. Using Origin Shield can help reduce the load on your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oId' - A unique identifier for the origin. This value must be unique within the distribution. Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or @DefaultCacheBehavior@ .
--
-- * 'oDomainName' - The domain name for the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ .
origin ::
  -- | 'oId'
  Text ->
  -- | 'oDomainName'
  Text ->
  Origin
origin pId_ pDomainName_ =
  Origin'
    { _oCustomHeaders = Nothing,
      _oCustomOriginConfig = Nothing,
      _oConnectionTimeout = Nothing,
      _oConnectionAttempts = Nothing,
      _oS3OriginConfig = Nothing,
      _oOriginPath = Nothing,
      _oOriginShield = Nothing,
      _oId = pId_,
      _oDomainName = pDomainName_
    }

-- | A list of HTTP header names and values that CloudFront adds to the requests that it sends to the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
oCustomHeaders :: Lens' Origin (Maybe CustomHeaders)
oCustomHeaders = lens _oCustomHeaders (\s a -> s {_oCustomHeaders = a})

-- | Use this type to specify an origin that is not an Amazon S3 bucket, with one exception. If the Amazon S3 bucket is configured with static website hosting, use this type. If the Amazon S3 bucket is not configured with static website hosting, use the @S3OriginConfig@ type instead.
oCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oCustomOriginConfig = lens _oCustomOriginConfig (\s a -> s {_oCustomOriginConfig = a})

-- | The number of seconds that CloudFront waits when trying to establish a connection to the origin. The minimum timeout is 1 second, the maximum is 10 seconds, and the default (if you don’t specify otherwise) is 10 seconds. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout> in the /Amazon CloudFront Developer Guide/ .
oConnectionTimeout :: Lens' Origin (Maybe Int)
oConnectionTimeout = lens _oConnectionTimeout (\s a -> s {_oConnectionTimeout = a})

-- | The number of times that CloudFront attempts to connect to the origin. The minimum number is 1, the maximum is 3, and the default (if you don’t specify otherwise) is 3. For a custom origin (including an Amazon S3 bucket that’s configured with static website hosting), this value also specifies the number of times that CloudFront attempts to get a response from the origin, in the case of an <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts> in the /Amazon CloudFront Developer Guide/ .
oConnectionAttempts :: Lens' Origin (Maybe Int)
oConnectionAttempts = lens _oConnectionAttempts (\s a -> s {_oConnectionAttempts = a})

-- | Use this type to specify an origin that is an Amazon S3 bucket that is not configured with static website hosting. To specify any other type of origin, including an Amazon S3 bucket that is configured with static website hosting, use the @CustomOriginConfig@ type instead.
oS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oS3OriginConfig = lens _oS3OriginConfig (\s a -> s {_oS3OriginConfig = a})

-- | An optional path that CloudFront appends to the origin domain name when CloudFront requests content from the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path> in the /Amazon CloudFront Developer Guide/ .
oOriginPath :: Lens' Origin (Maybe Text)
oOriginPath = lens _oOriginPath (\s a -> s {_oOriginPath = a})

-- | CloudFront Origin Shield. Using Origin Shield can help reduce the load on your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
oOriginShield :: Lens' Origin (Maybe OriginShield)
oOriginShield = lens _oOriginShield (\s a -> s {_oOriginShield = a})

-- | A unique identifier for the origin. This value must be unique within the distribution. Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or @DefaultCacheBehavior@ .
oId :: Lens' Origin Text
oId = lens _oId (\s a -> s {_oId = a})

-- | The domain name for the origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ .
oDomainName :: Lens' Origin Text
oDomainName = lens _oDomainName (\s a -> s {_oDomainName = a})

instance FromXML Origin where
  parseXML x =
    Origin'
      <$> (x .@? "CustomHeaders")
      <*> (x .@? "CustomOriginConfig")
      <*> (x .@? "ConnectionTimeout")
      <*> (x .@? "ConnectionAttempts")
      <*> (x .@? "S3OriginConfig")
      <*> (x .@? "OriginPath")
      <*> (x .@? "OriginShield")
      <*> (x .@ "Id")
      <*> (x .@ "DomainName")

instance Hashable Origin

instance NFData Origin

instance ToXML Origin where
  toXML Origin' {..} =
    mconcat
      [ "CustomHeaders" @= _oCustomHeaders,
        "CustomOriginConfig" @= _oCustomOriginConfig,
        "ConnectionTimeout" @= _oConnectionTimeout,
        "ConnectionAttempts" @= _oConnectionAttempts,
        "S3OriginConfig" @= _oS3OriginConfig,
        "OriginPath" @= _oOriginPath,
        "OriginShield" @= _oOriginShield,
        "Id" @= _oId,
        "DomainName" @= _oDomainName
      ]
