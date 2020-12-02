{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionSummary where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of the information for a CloudFront streaming distribution.
--
--
--
-- /See:/ 'streamingDistributionSummary' smart constructor.
data StreamingDistributionSummary = StreamingDistributionSummary'
  { _sdsId ::
      !Text,
    _sdsARN :: !Text,
    _sdsStatus :: !Text,
    _sdsLastModifiedTime :: !ISO8601,
    _sdsDomainName :: !Text,
    _sdsS3Origin :: !S3Origin,
    _sdsAliases :: !Aliases,
    _sdsTrustedSigners ::
      !TrustedSigners,
    _sdsComment :: !Text,
    _sdsPriceClass :: !PriceClass,
    _sdsEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingDistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsId' - The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
--
-- * 'sdsARN' - The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'sdsStatus' - Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
--
-- * 'sdsLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'sdsDomainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'sdsS3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- * 'sdsAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- * 'sdsTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'sdsComment' - The comment originally specified when this distribution was created.
--
-- * 'sdsPriceClass' - A complex type that contains information about price class for this streaming distribution.
--
-- * 'sdsEnabled' - Whether the distribution is enabled to accept end user requests for content.
streamingDistributionSummary ::
  -- | 'sdsId'
  Text ->
  -- | 'sdsARN'
  Text ->
  -- | 'sdsStatus'
  Text ->
  -- | 'sdsLastModifiedTime'
  UTCTime ->
  -- | 'sdsDomainName'
  Text ->
  -- | 'sdsS3Origin'
  S3Origin ->
  -- | 'sdsAliases'
  Aliases ->
  -- | 'sdsTrustedSigners'
  TrustedSigners ->
  -- | 'sdsComment'
  Text ->
  -- | 'sdsPriceClass'
  PriceClass ->
  -- | 'sdsEnabled'
  Bool ->
  StreamingDistributionSummary
streamingDistributionSummary
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pDomainName_
  pS3Origin_
  pAliases_
  pTrustedSigners_
  pComment_
  pPriceClass_
  pEnabled_ =
    StreamingDistributionSummary'
      { _sdsId = pId_,
        _sdsARN = pARN_,
        _sdsStatus = pStatus_,
        _sdsLastModifiedTime = _Time # pLastModifiedTime_,
        _sdsDomainName = pDomainName_,
        _sdsS3Origin = pS3Origin_,
        _sdsAliases = pAliases_,
        _sdsTrustedSigners = pTrustedSigners_,
        _sdsComment = pComment_,
        _sdsPriceClass = pPriceClass_,
        _sdsEnabled = pEnabled_
      }

-- | The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
sdsId :: Lens' StreamingDistributionSummary Text
sdsId = lens _sdsId (\s a -> s {_sdsId = a})

-- | The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
sdsARN :: Lens' StreamingDistributionSummary Text
sdsARN = lens _sdsARN (\s a -> s {_sdsARN = a})

-- | Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary Text
sdsStatus = lens _sdsStatus (\s a -> s {_sdsStatus = a})

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary UTCTime
sdsLastModifiedTime = lens _sdsLastModifiedTime (\s a -> s {_sdsLastModifiedTime = a}) . _Time

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
sdsDomainName :: Lens' StreamingDistributionSummary Text
sdsDomainName = lens _sdsDomainName (\s a -> s {_sdsDomainName = a})

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = lens _sdsS3Origin (\s a -> s {_sdsS3Origin = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary Aliases
sdsAliases = lens _sdsAliases (\s a -> s {_sdsAliases = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
sdsTrustedSigners :: Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners = lens _sdsTrustedSigners (\s a -> s {_sdsTrustedSigners = a})

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary Text
sdsComment = lens _sdsComment (\s a -> s {_sdsComment = a})

-- | A complex type that contains information about price class for this streaming distribution.
sdsPriceClass :: Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = lens _sdsPriceClass (\s a -> s {_sdsPriceClass = a})

-- | Whether the distribution is enabled to accept end user requests for content.
sdsEnabled :: Lens' StreamingDistributionSummary Bool
sdsEnabled = lens _sdsEnabled (\s a -> s {_sdsEnabled = a})

instance FromXML StreamingDistributionSummary where
  parseXML x =
    StreamingDistributionSummary'
      <$> (x .@ "Id")
      <*> (x .@ "ARN")
      <*> (x .@ "Status")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "DomainName")
      <*> (x .@ "S3Origin")
      <*> (x .@ "Aliases")
      <*> (x .@ "TrustedSigners")
      <*> (x .@ "Comment")
      <*> (x .@ "PriceClass")
      <*> (x .@ "Enabled")

instance Hashable StreamingDistributionSummary

instance NFData StreamingDistributionSummary
