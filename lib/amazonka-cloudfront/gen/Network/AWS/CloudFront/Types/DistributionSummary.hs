{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionSummary where

import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.HTTPVersion
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.ViewerCertificate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of the information about a CloudFront distribution.
--
--
--
-- /See:/ 'distributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { _dsOriginGroups ::
      !(Maybe OriginGroups),
    _dsAliasICPRecordals :: !(Maybe [AliasICPRecordal]),
    _dsId :: !Text,
    _dsARN :: !Text,
    _dsStatus :: !Text,
    _dsLastModifiedTime :: !ISO8601,
    _dsDomainName :: !Text,
    _dsAliases :: !Aliases,
    _dsOrigins :: !Origins,
    _dsDefaultCacheBehavior :: !DefaultCacheBehavior,
    _dsCacheBehaviors :: !CacheBehaviors,
    _dsCustomErrorResponses :: !CustomErrorResponses,
    _dsComment :: !Text,
    _dsPriceClass :: !PriceClass,
    _dsEnabled :: !Bool,
    _dsViewerCertificate :: !ViewerCertificate,
    _dsRestrictions :: !Restrictions,
    _dsWebACLId :: !Text,
    _dsHTTPVersion :: !HTTPVersion,
    _dsIsIPV6Enabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsOriginGroups' - A complex type that contains information about origin groups for this distribution.
--
-- * 'dsAliasICPRecordals' - AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- * 'dsId' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- * 'dsARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'dsStatus' - The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- * 'dsLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'dsDomainName' - The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'dsAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- * 'dsOrigins' - A complex type that contains information about origins for this distribution.
--
-- * 'dsDefaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- * 'dsCacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- * 'dsCustomErrorResponses' - A complex type that contains zero or more @CustomErrorResponses@ elements.
--
-- * 'dsComment' - The comment originally specified when this distribution was created.
--
-- * 'dsPriceClass' - A complex type that contains information about price class for this streaming distribution.
--
-- * 'dsEnabled' - Whether the distribution is enabled to accept user requests for content.
--
-- * 'dsViewerCertificate' - A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
--
-- * 'dsRestrictions' - A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- * 'dsWebACLId' - The Web ACL Id (if any) associated with the distribution.
--
-- * 'dsHTTPVersion' - Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
--
-- * 'dsIsIPV6Enabled' - Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
distributionSummary ::
  -- | 'dsId'
  Text ->
  -- | 'dsARN'
  Text ->
  -- | 'dsStatus'
  Text ->
  -- | 'dsLastModifiedTime'
  UTCTime ->
  -- | 'dsDomainName'
  Text ->
  -- | 'dsAliases'
  Aliases ->
  -- | 'dsOrigins'
  Origins ->
  -- | 'dsDefaultCacheBehavior'
  DefaultCacheBehavior ->
  -- | 'dsCacheBehaviors'
  CacheBehaviors ->
  -- | 'dsCustomErrorResponses'
  CustomErrorResponses ->
  -- | 'dsComment'
  Text ->
  -- | 'dsPriceClass'
  PriceClass ->
  -- | 'dsEnabled'
  Bool ->
  -- | 'dsViewerCertificate'
  ViewerCertificate ->
  -- | 'dsRestrictions'
  Restrictions ->
  -- | 'dsWebACLId'
  Text ->
  -- | 'dsHTTPVersion'
  HTTPVersion ->
  -- | 'dsIsIPV6Enabled'
  Bool ->
  DistributionSummary
distributionSummary
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pDomainName_
  pAliases_
  pOrigins_
  pDefaultCacheBehavior_
  pCacheBehaviors_
  pCustomErrorResponses_
  pComment_
  pPriceClass_
  pEnabled_
  pViewerCertificate_
  pRestrictions_
  pWebACLId_
  pHTTPVersion_
  pIsIPV6Enabled_ =
    DistributionSummary'
      { _dsOriginGroups = Nothing,
        _dsAliasICPRecordals = Nothing,
        _dsId = pId_,
        _dsARN = pARN_,
        _dsStatus = pStatus_,
        _dsLastModifiedTime = _Time # pLastModifiedTime_,
        _dsDomainName = pDomainName_,
        _dsAliases = pAliases_,
        _dsOrigins = pOrigins_,
        _dsDefaultCacheBehavior = pDefaultCacheBehavior_,
        _dsCacheBehaviors = pCacheBehaviors_,
        _dsCustomErrorResponses = pCustomErrorResponses_,
        _dsComment = pComment_,
        _dsPriceClass = pPriceClass_,
        _dsEnabled = pEnabled_,
        _dsViewerCertificate = pViewerCertificate_,
        _dsRestrictions = pRestrictions_,
        _dsWebACLId = pWebACLId_,
        _dsHTTPVersion = pHTTPVersion_,
        _dsIsIPV6Enabled = pIsIPV6Enabled_
      }

-- | A complex type that contains information about origin groups for this distribution.
dsOriginGroups :: Lens' DistributionSummary (Maybe OriginGroups)
dsOriginGroups = lens _dsOriginGroups (\s a -> s {_dsOriginGroups = a})

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
dsAliasICPRecordals :: Lens' DistributionSummary [AliasICPRecordal]
dsAliasICPRecordals = lens _dsAliasICPRecordals (\s a -> s {_dsAliasICPRecordals = a}) . _Default . _Coerce

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
dsId :: Lens' DistributionSummary Text
dsId = lens _dsId (\s a -> s {_dsId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
dsARN :: Lens' DistributionSummary Text
dsARN = lens _dsARN (\s a -> s {_dsARN = a})

-- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
dsStatus :: Lens' DistributionSummary Text
dsStatus = lens _dsStatus (\s a -> s {_dsStatus = a})

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary UTCTime
dsLastModifiedTime = lens _dsLastModifiedTime (\s a -> s {_dsLastModifiedTime = a}) . _Time

-- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
dsDomainName :: Lens' DistributionSummary Text
dsDomainName = lens _dsDomainName (\s a -> s {_dsDomainName = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary Aliases
dsAliases = lens _dsAliases (\s a -> s {_dsAliases = a})

-- | A complex type that contains information about origins for this distribution.
dsOrigins :: Lens' DistributionSummary Origins
dsOrigins = lens _dsOrigins (\s a -> s {_dsOrigins = a})

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior = lens _dsDefaultCacheBehavior (\s a -> s {_dsDefaultCacheBehavior = a})

-- | A complex type that contains zero or more @CacheBehavior@ elements.
dsCacheBehaviors :: Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = lens _dsCacheBehaviors (\s a -> s {_dsCacheBehaviors = a})

-- | A complex type that contains zero or more @CustomErrorResponses@ elements.
dsCustomErrorResponses :: Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses = lens _dsCustomErrorResponses (\s a -> s {_dsCustomErrorResponses = a})

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary Text
dsComment = lens _dsComment (\s a -> s {_dsComment = a})

-- | A complex type that contains information about price class for this streaming distribution.
dsPriceClass :: Lens' DistributionSummary PriceClass
dsPriceClass = lens _dsPriceClass (\s a -> s {_dsPriceClass = a})

-- | Whether the distribution is enabled to accept user requests for content.
dsEnabled :: Lens' DistributionSummary Bool
dsEnabled = lens _dsEnabled (\s a -> s {_dsEnabled = a})

-- | A complex type that determines the distribution’s SSL/TLS configuration for communicating with viewers.
dsViewerCertificate :: Lens' DistributionSummary ViewerCertificate
dsViewerCertificate = lens _dsViewerCertificate (\s a -> s {_dsViewerCertificate = a})

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
dsRestrictions :: Lens' DistributionSummary Restrictions
dsRestrictions = lens _dsRestrictions (\s a -> s {_dsRestrictions = a})

-- | The Web ACL Id (if any) associated with the distribution.
dsWebACLId :: Lens' DistributionSummary Text
dsWebACLId = lens _dsWebACLId (\s a -> s {_dsWebACLId = a})

-- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
dsHTTPVersion :: Lens' DistributionSummary HTTPVersion
dsHTTPVersion = lens _dsHTTPVersion (\s a -> s {_dsHTTPVersion = a})

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
dsIsIPV6Enabled :: Lens' DistributionSummary Bool
dsIsIPV6Enabled = lens _dsIsIPV6Enabled (\s a -> s {_dsIsIPV6Enabled = a})

instance FromXML DistributionSummary where
  parseXML x =
    DistributionSummary'
      <$> (x .@? "OriginGroups")
      <*> ( x .@? "AliasICPRecordals" .!@ mempty
              >>= may (parseXMLList "AliasICPRecordal")
          )
      <*> (x .@ "Id")
      <*> (x .@ "ARN")
      <*> (x .@ "Status")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "DomainName")
      <*> (x .@ "Aliases")
      <*> (x .@ "Origins")
      <*> (x .@ "DefaultCacheBehavior")
      <*> (x .@ "CacheBehaviors")
      <*> (x .@ "CustomErrorResponses")
      <*> (x .@ "Comment")
      <*> (x .@ "PriceClass")
      <*> (x .@ "Enabled")
      <*> (x .@ "ViewerCertificate")
      <*> (x .@ "Restrictions")
      <*> (x .@ "WebACLId")
      <*> (x .@ "HttpVersion")
      <*> (x .@ "IsIPV6Enabled")

instance Hashable DistributionSummary

instance NFData DistributionSummary
