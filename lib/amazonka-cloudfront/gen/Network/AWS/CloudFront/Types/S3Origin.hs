{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.S3Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3Origin where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
--
--
-- /See:/ 's3Origin' smart constructor.
data S3Origin = S3Origin'
  { _soDomainName :: !Text,
    _soOriginAccessIdentity :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soDomainName' - The DNS name of the Amazon S3 origin.
--
-- * 'soOriginAccessIdentity' - The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront. If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
s3Origin ::
  -- | 'soDomainName'
  Text ->
  -- | 'soOriginAccessIdentity'
  Text ->
  S3Origin
s3Origin pDomainName_ pOriginAccessIdentity_ =
  S3Origin'
    { _soDomainName = pDomainName_,
      _soOriginAccessIdentity = pOriginAccessIdentity_
    }

-- | The DNS name of the Amazon S3 origin.
soDomainName :: Lens' S3Origin Text
soDomainName = lens _soDomainName (\s a -> s {_soDomainName = a})

-- | The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront. If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
soOriginAccessIdentity :: Lens' S3Origin Text
soOriginAccessIdentity = lens _soOriginAccessIdentity (\s a -> s {_soOriginAccessIdentity = a})

instance FromXML S3Origin where
  parseXML x =
    S3Origin'
      <$> (x .@ "DomainName") <*> (x .@ "OriginAccessIdentity")

instance Hashable S3Origin

instance NFData S3Origin

instance ToXML S3Origin where
  toXML S3Origin' {..} =
    mconcat
      [ "DomainName" @= _soDomainName,
        "OriginAccessIdentity" @= _soOriginAccessIdentity
      ]
