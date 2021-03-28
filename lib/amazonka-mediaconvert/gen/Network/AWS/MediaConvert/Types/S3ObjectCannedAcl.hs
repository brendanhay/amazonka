{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ObjectCannedAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.S3ObjectCannedAcl
  ( S3ObjectCannedAcl
    ( S3ObjectCannedAcl'
    , S3ObjectCannedAclPublicRead
    , S3ObjectCannedAclAuthenticatedRead
    , S3ObjectCannedAclBucketOwnerRead
    , S3ObjectCannedAclBucketOwnerFullControl
    , fromS3ObjectCannedAcl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
newtype S3ObjectCannedAcl = S3ObjectCannedAcl'{fromS3ObjectCannedAcl
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern S3ObjectCannedAclPublicRead :: S3ObjectCannedAcl
pattern S3ObjectCannedAclPublicRead = S3ObjectCannedAcl' "PUBLIC_READ"

pattern S3ObjectCannedAclAuthenticatedRead :: S3ObjectCannedAcl
pattern S3ObjectCannedAclAuthenticatedRead = S3ObjectCannedAcl' "AUTHENTICATED_READ"

pattern S3ObjectCannedAclBucketOwnerRead :: S3ObjectCannedAcl
pattern S3ObjectCannedAclBucketOwnerRead = S3ObjectCannedAcl' "BUCKET_OWNER_READ"

pattern S3ObjectCannedAclBucketOwnerFullControl :: S3ObjectCannedAcl
pattern S3ObjectCannedAclBucketOwnerFullControl = S3ObjectCannedAcl' "BUCKET_OWNER_FULL_CONTROL"

{-# COMPLETE 
  S3ObjectCannedAclPublicRead,

  S3ObjectCannedAclAuthenticatedRead,

  S3ObjectCannedAclBucketOwnerRead,

  S3ObjectCannedAclBucketOwnerFullControl,
  S3ObjectCannedAcl'
  #-}
