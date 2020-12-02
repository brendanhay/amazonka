{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistribution where

import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A streaming distribution tells CloudFront where you want RTMP content to be delivered from, and the details about how to track and manage content delivery.
--
--
--
-- /See:/ 'streamingDistribution' smart constructor.
data StreamingDistribution = StreamingDistribution'
  { _sdLastModifiedTime ::
      !(Maybe ISO8601),
    _sdId :: !Text,
    _sdARN :: !Text,
    _sdStatus :: !Text,
    _sdDomainName :: !Text,
    _sdActiveTrustedSigners ::
      !ActiveTrustedSigners,
    _sdStreamingDistributionConfig ::
      !StreamingDistributionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLastModifiedTime' - The date and time that the distribution was last modified.
--
-- * 'sdId' - The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
--
-- * 'sdARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'sdStatus' - The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- * 'sdDomainName' - The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
--
-- * 'sdActiveTrustedSigners' - A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content. The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'sdStreamingDistributionConfig' - The current configuration information for the RTMP distribution.
streamingDistribution ::
  -- | 'sdId'
  Text ->
  -- | 'sdARN'
  Text ->
  -- | 'sdStatus'
  Text ->
  -- | 'sdDomainName'
  Text ->
  -- | 'sdActiveTrustedSigners'
  ActiveTrustedSigners ->
  -- | 'sdStreamingDistributionConfig'
  StreamingDistributionConfig ->
  StreamingDistribution
streamingDistribution
  pId_
  pARN_
  pStatus_
  pDomainName_
  pActiveTrustedSigners_
  pStreamingDistributionConfig_ =
    StreamingDistribution'
      { _sdLastModifiedTime = Nothing,
        _sdId = pId_,
        _sdARN = pARN_,
        _sdStatus = pStatus_,
        _sdDomainName = pDomainName_,
        _sdActiveTrustedSigners = pActiveTrustedSigners_,
        _sdStreamingDistributionConfig = pStreamingDistributionConfig_
      }

-- | The date and time that the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe UTCTime)
sdLastModifiedTime = lens _sdLastModifiedTime (\s a -> s {_sdLastModifiedTime = a}) . mapping _Time

-- | The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
sdId :: Lens' StreamingDistribution Text
sdId = lens _sdId (\s a -> s {_sdId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
sdARN :: Lens' StreamingDistribution Text
sdARN = lens _sdARN (\s a -> s {_sdARN = a})

-- | The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
sdStatus :: Lens' StreamingDistribution Text
sdStatus = lens _sdStatus (\s a -> s {_sdStatus = a})

-- | The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
sdDomainName :: Lens' StreamingDistribution Text
sdDomainName = lens _sdDomainName (\s a -> s {_sdDomainName = a})

-- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content. The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
sdActiveTrustedSigners :: Lens' StreamingDistribution ActiveTrustedSigners
sdActiveTrustedSigners = lens _sdActiveTrustedSigners (\s a -> s {_sdActiveTrustedSigners = a})

-- | The current configuration information for the RTMP distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution StreamingDistributionConfig
sdStreamingDistributionConfig = lens _sdStreamingDistributionConfig (\s a -> s {_sdStreamingDistributionConfig = a})

instance FromXML StreamingDistribution where
  parseXML x =
    StreamingDistribution'
      <$> (x .@? "LastModifiedTime")
      <*> (x .@ "Id")
      <*> (x .@ "ARN")
      <*> (x .@ "Status")
      <*> (x .@ "DomainName")
      <*> (x .@ "ActiveTrustedSigners")
      <*> (x .@ "StreamingDistributionConfig")

instance Hashable StreamingDistribution

instance NFData StreamingDistribution
