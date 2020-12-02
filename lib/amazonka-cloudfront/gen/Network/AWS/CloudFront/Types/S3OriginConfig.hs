{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.S3OriginConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3OriginConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin or an S3 bucket that is configured as a website endpoint, use the @CustomOriginConfig@ element instead.
--
--
--
-- /See:/ 's3OriginConfig' smart constructor.
newtype S3OriginConfig = S3OriginConfig'
  { _socOriginAccessIdentity ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3OriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socOriginAccessIdentity' - The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is: origin-access-identity/cloudfront//ID-of-origin-access-identity/  where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity. If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information about the origin access identity, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
s3OriginConfig ::
  -- | 'socOriginAccessIdentity'
  Text ->
  S3OriginConfig
s3OriginConfig pOriginAccessIdentity_ =
  S3OriginConfig'
    { _socOriginAccessIdentity =
        pOriginAccessIdentity_
    }

-- | The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is: origin-access-identity/cloudfront//ID-of-origin-access-identity/  where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity. If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information about the origin access identity, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
socOriginAccessIdentity :: Lens' S3OriginConfig Text
socOriginAccessIdentity = lens _socOriginAccessIdentity (\s a -> s {_socOriginAccessIdentity = a})

instance FromXML S3OriginConfig where
  parseXML x = S3OriginConfig' <$> (x .@ "OriginAccessIdentity")

instance Hashable S3OriginConfig

instance NFData S3OriginConfig

instance ToXML S3OriginConfig where
  toXML S3OriginConfig' {..} =
    mconcat ["OriginAccessIdentity" @= _socOriginAccessIdentity]
