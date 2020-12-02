{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary of the information about a CloudFront origin access identity.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
  { _cfoaisId ::
      !Text,
    _cfoaisS3CanonicalUserId ::
      !Text,
    _cfoaisComment ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentitySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaisId' - The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
--
-- * 'cfoaisS3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
--
-- * 'cfoaisComment' - The comment for this origin access identity, as originally specified when created.
cloudFrontOriginAccessIdentitySummary ::
  -- | 'cfoaisId'
  Text ->
  -- | 'cfoaisS3CanonicalUserId'
  Text ->
  -- | 'cfoaisComment'
  Text ->
  CloudFrontOriginAccessIdentitySummary
cloudFrontOriginAccessIdentitySummary
  pId_
  pS3CanonicalUserId_
  pComment_ =
    CloudFrontOriginAccessIdentitySummary'
      { _cfoaisId = pId_,
        _cfoaisS3CanonicalUserId = pS3CanonicalUserId_,
        _cfoaisComment = pComment_
      }

-- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisId = lens _cfoaisId (\s a -> s {_cfoaisId = a})

-- | The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisS3CanonicalUserId = lens _cfoaisS3CanonicalUserId (\s a -> s {_cfoaisS3CanonicalUserId = a})

-- | The comment for this origin access identity, as originally specified when created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisComment = lens _cfoaisComment (\s a -> s {_cfoaisComment = a})

instance FromXML CloudFrontOriginAccessIdentitySummary where
  parseXML x =
    CloudFrontOriginAccessIdentitySummary'
      <$> (x .@ "Id") <*> (x .@ "S3CanonicalUserId") <*> (x .@ "Comment")

instance Hashable CloudFrontOriginAccessIdentitySummary

instance NFData CloudFrontOriginAccessIdentitySummary
