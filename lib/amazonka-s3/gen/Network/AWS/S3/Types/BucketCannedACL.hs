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
        BucketCannedACLBPrivate,
        BucketCannedACLBPublicRead,
        BucketCannedACLBPublicReadWrite,
        BucketCannedACLBAuthenticatedRead,
        fromBucketCannedACL
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

newtype BucketCannedACL = BucketCannedACL'
  { fromBucketCannedACL ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BucketCannedACLBPrivate :: BucketCannedACL
pattern BucketCannedACLBPrivate = BucketCannedACL' "private"

pattern BucketCannedACLBPublicRead :: BucketCannedACL
pattern BucketCannedACLBPublicRead = BucketCannedACL' "public-read"

pattern BucketCannedACLBPublicReadWrite :: BucketCannedACL
pattern BucketCannedACLBPublicReadWrite = BucketCannedACL' "public-read-write"

pattern BucketCannedACLBAuthenticatedRead :: BucketCannedACL
pattern BucketCannedACLBAuthenticatedRead = BucketCannedACL' "authenticated-read"

{-# COMPLETE
  BucketCannedACLBPrivate,
  BucketCannedACLBPublicRead,
  BucketCannedACLBPublicReadWrite,
  BucketCannedACLBAuthenticatedRead,
  BucketCannedACL'
  #-}
