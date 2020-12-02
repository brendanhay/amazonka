{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | CloudFront origin access identity.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
  { _cfoaiCloudFrontOriginAccessIdentityConfig ::
      !( Maybe
           CloudFrontOriginAccessIdentityConfig
       ),
    _cfoaiId :: !Text,
    _cfoaiS3CanonicalUserId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaiCloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
--
-- * 'cfoaiId' - The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
--
-- * 'cfoaiS3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
cloudFrontOriginAccessIdentity ::
  -- | 'cfoaiId'
  Text ->
  -- | 'cfoaiS3CanonicalUserId'
  Text ->
  CloudFrontOriginAccessIdentity
cloudFrontOriginAccessIdentity pId_ pS3CanonicalUserId_ =
  CloudFrontOriginAccessIdentity'
    { _cfoaiCloudFrontOriginAccessIdentityConfig =
        Nothing,
      _cfoaiId = pId_,
      _cfoaiS3CanonicalUserId = pS3CanonicalUserId_
    }

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = lens _cfoaiCloudFrontOriginAccessIdentityConfig (\s a -> s {_cfoaiCloudFrontOriginAccessIdentityConfig = a})

-- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
cfoaiId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiId = lens _cfoaiId (\s a -> s {_cfoaiId = a})

-- | The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiS3CanonicalUserId = lens _cfoaiS3CanonicalUserId (\s a -> s {_cfoaiS3CanonicalUserId = a})

instance FromXML CloudFrontOriginAccessIdentity where
  parseXML x =
    CloudFrontOriginAccessIdentity'
      <$> (x .@? "CloudFrontOriginAccessIdentityConfig")
      <*> (x .@ "Id")
      <*> (x .@ "S3CanonicalUserId")

instance Hashable CloudFrontOriginAccessIdentity

instance NFData CloudFrontOriginAccessIdentity
