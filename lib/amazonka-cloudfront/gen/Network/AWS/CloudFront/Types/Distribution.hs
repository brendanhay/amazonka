{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Distribution where

import Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.DistributionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A distribution tells CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery.
--
--
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'
  { _dActiveTrustedKeyGroups ::
      !(Maybe ActiveTrustedKeyGroups),
    _dAliasICPRecordals :: !(Maybe [AliasICPRecordal]),
    _dActiveTrustedSigners :: !(Maybe ActiveTrustedSigners),
    _dId :: !Text,
    _dARN :: !Text,
    _dStatus :: !Text,
    _dLastModifiedTime :: !ISO8601,
    _dInProgressInvalidationBatches :: !Int,
    _dDomainName :: !Text,
    _dDistributionConfig :: !DistributionConfig
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dActiveTrustedKeyGroups' - CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- * 'dAliasICPRecordals' - AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- * 'dActiveTrustedSigners' - /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ . CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
--
-- * 'dId' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- * 'dARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'dStatus' - This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
--
-- * 'dLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'dInProgressInvalidationBatches' - The number of invalidation batches currently in progress.
--
-- * 'dDomainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'dDistributionConfig' - The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
distribution ::
  -- | 'dId'
  Text ->
  -- | 'dARN'
  Text ->
  -- | 'dStatus'
  Text ->
  -- | 'dLastModifiedTime'
  UTCTime ->
  -- | 'dInProgressInvalidationBatches'
  Int ->
  -- | 'dDomainName'
  Text ->
  -- | 'dDistributionConfig'
  DistributionConfig ->
  Distribution
distribution
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pInProgressInvalidationBatches_
  pDomainName_
  pDistributionConfig_ =
    Distribution'
      { _dActiveTrustedKeyGroups = Nothing,
        _dAliasICPRecordals = Nothing,
        _dActiveTrustedSigners = Nothing,
        _dId = pId_,
        _dARN = pARN_,
        _dStatus = pStatus_,
        _dLastModifiedTime = _Time # pLastModifiedTime_,
        _dInProgressInvalidationBatches = pInProgressInvalidationBatches_,
        _dDomainName = pDomainName_,
        _dDistributionConfig = pDistributionConfig_
      }

-- | CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using key groups. This field contains a list of key groups and the public keys in each key group that CloudFront can use to verify the signatures of signed URLs or signed cookies.
dActiveTrustedKeyGroups :: Lens' Distribution (Maybe ActiveTrustedKeyGroups)
dActiveTrustedKeyGroups = lens _dActiveTrustedKeyGroups (\s a -> s {_dActiveTrustedKeyGroups = a})

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
dAliasICPRecordals :: Lens' Distribution [AliasICPRecordal]
dAliasICPRecordals = lens _dAliasICPRecordals (\s a -> s {_dAliasICPRecordals = a}) . _Default . _Coerce

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ . CloudFront automatically adds this field to the response if you’ve configured a cache behavior in this distribution to serve private content using trusted signers. This field contains a list of AWS account IDs and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs or signed cookies.
dActiveTrustedSigners :: Lens' Distribution (Maybe ActiveTrustedSigners)
dActiveTrustedSigners = lens _dActiveTrustedSigners (\s a -> s {_dActiveTrustedSigners = a})

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
dId :: Lens' Distribution Text
dId = lens _dId (\s a -> s {_dId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
dARN :: Lens' Distribution Text
dARN = lens _dARN (\s a -> s {_dARN = a})

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
dStatus :: Lens' Distribution Text
dStatus = lens _dStatus (\s a -> s {_dStatus = a})

-- | The date and time the distribution was last modified.
dLastModifiedTime :: Lens' Distribution UTCTime
dLastModifiedTime = lens _dLastModifiedTime (\s a -> s {_dLastModifiedTime = a}) . _Time

-- | The number of invalidation batches currently in progress.
dInProgressInvalidationBatches :: Lens' Distribution Int
dInProgressInvalidationBatches = lens _dInProgressInvalidationBatches (\s a -> s {_dInProgressInvalidationBatches = a})

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
dDomainName :: Lens' Distribution Text
dDomainName = lens _dDomainName (\s a -> s {_dDomainName = a})

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
dDistributionConfig :: Lens' Distribution DistributionConfig
dDistributionConfig = lens _dDistributionConfig (\s a -> s {_dDistributionConfig = a})

instance FromXML Distribution where
  parseXML x =
    Distribution'
      <$> (x .@? "ActiveTrustedKeyGroups")
      <*> ( x .@? "AliasICPRecordals" .!@ mempty
              >>= may (parseXMLList "AliasICPRecordal")
          )
      <*> (x .@? "ActiveTrustedSigners")
      <*> (x .@ "Id")
      <*> (x .@ "ARN")
      <*> (x .@ "Status")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "InProgressInvalidationBatches")
      <*> (x .@ "DomainName")
      <*> (x .@ "DistributionConfig")

instance Hashable Distribution

instance NFData Distribution
