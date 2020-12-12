{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectCannedACL
  ( ObjectCannedACL
      ( ObjectCannedACL',
        OAWSExecRead,
        OAuthenticatedRead,
        OBucketOwnerFullControl,
        OBucketOwnerRead,
        OPrivate,
        OPublicRead,
        OPublicReadWrite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype ObjectCannedACL = ObjectCannedACL' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OAWSExecRead :: ObjectCannedACL
pattern OAWSExecRead = ObjectCannedACL' "aws-exec-read"

pattern OAuthenticatedRead :: ObjectCannedACL
pattern OAuthenticatedRead = ObjectCannedACL' "authenticated-read"

pattern OBucketOwnerFullControl :: ObjectCannedACL
pattern OBucketOwnerFullControl = ObjectCannedACL' "bucket-owner-full-control"

pattern OBucketOwnerRead :: ObjectCannedACL
pattern OBucketOwnerRead = ObjectCannedACL' "bucket-owner-read"

pattern OPrivate :: ObjectCannedACL
pattern OPrivate = ObjectCannedACL' "private"

pattern OPublicRead :: ObjectCannedACL
pattern OPublicRead = ObjectCannedACL' "public-read"

pattern OPublicReadWrite :: ObjectCannedACL
pattern OPublicReadWrite = ObjectCannedACL' "public-read-write"

{-# COMPLETE
  OAWSExecRead,
  OAuthenticatedRead,
  OBucketOwnerFullControl,
  OBucketOwnerRead,
  OPrivate,
  OPublicRead,
  OPublicReadWrite,
  ObjectCannedACL'
  #-}
