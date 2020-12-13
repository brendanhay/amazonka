{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ObjectCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3ObjectCannedACL
  ( S3ObjectCannedACL
      ( S3ObjectCannedACL',
        PublicRead,
        AuthenticatedRead,
        BucketOwnerRead,
        BucketOwnerFullControl
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
newtype S3ObjectCannedACL = S3ObjectCannedACL' Lude.Text
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

pattern PublicRead :: S3ObjectCannedACL
pattern PublicRead = S3ObjectCannedACL' "PUBLIC_READ"

pattern AuthenticatedRead :: S3ObjectCannedACL
pattern AuthenticatedRead = S3ObjectCannedACL' "AUTHENTICATED_READ"

pattern BucketOwnerRead :: S3ObjectCannedACL
pattern BucketOwnerRead = S3ObjectCannedACL' "BUCKET_OWNER_READ"

pattern BucketOwnerFullControl :: S3ObjectCannedACL
pattern BucketOwnerFullControl = S3ObjectCannedACL' "BUCKET_OWNER_FULL_CONTROL"

{-# COMPLETE
  PublicRead,
  AuthenticatedRead,
  BucketOwnerRead,
  BucketOwnerFullControl,
  S3ObjectCannedACL'
  #-}
