{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketCannedACL
  ( BucketCannedACL
      ( BucketCannedACL',
        BPrivate,
        BPublicRead,
        BPublicReadWrite,
        BAuthenticatedRead
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype BucketCannedACL = BucketCannedACL' Lude.Text
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

pattern BPrivate :: BucketCannedACL
pattern BPrivate = BucketCannedACL' "private"

pattern BPublicRead :: BucketCannedACL
pattern BPublicRead = BucketCannedACL' "public-read"

pattern BPublicReadWrite :: BucketCannedACL
pattern BPublicReadWrite = BucketCannedACL' "public-read-write"

pattern BAuthenticatedRead :: BucketCannedACL
pattern BAuthenticatedRead = BucketCannedACL' "authenticated-read"

{-# COMPLETE
  BPrivate,
  BPublicRead,
  BPublicReadWrite,
  BAuthenticatedRead,
  BucketCannedACL'
  #-}
