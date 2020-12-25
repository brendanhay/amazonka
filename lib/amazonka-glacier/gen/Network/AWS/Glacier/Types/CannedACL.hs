{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CannedACL
  ( CannedACL
      ( CannedACL',
        CannedACLPrivate,
        CannedACLPublicRead,
        CannedACLPublicReadWrite,
        CannedACLAwsExecRead,
        CannedACLAuthenticatedRead,
        CannedACLBucketOwnerRead,
        CannedACLBucketOwnerFullControl,
        fromCannedACL
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CannedACL = CannedACL' {fromCannedACL :: Core.Text}
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

pattern CannedACLPrivate :: CannedACL
pattern CannedACLPrivate = CannedACL' "private"

pattern CannedACLPublicRead :: CannedACL
pattern CannedACLPublicRead = CannedACL' "public-read"

pattern CannedACLPublicReadWrite :: CannedACL
pattern CannedACLPublicReadWrite = CannedACL' "public-read-write"

pattern CannedACLAwsExecRead :: CannedACL
pattern CannedACLAwsExecRead = CannedACL' "aws-exec-read"

pattern CannedACLAuthenticatedRead :: CannedACL
pattern CannedACLAuthenticatedRead = CannedACL' "authenticated-read"

pattern CannedACLBucketOwnerRead :: CannedACL
pattern CannedACLBucketOwnerRead = CannedACL' "bucket-owner-read"

pattern CannedACLBucketOwnerFullControl :: CannedACL
pattern CannedACLBucketOwnerFullControl = CannedACL' "bucket-owner-full-control"

{-# COMPLETE
  CannedACLPrivate,
  CannedACLPublicRead,
  CannedACLPublicReadWrite,
  CannedACLAwsExecRead,
  CannedACLAuthenticatedRead,
  CannedACLBucketOwnerRead,
  CannedACLBucketOwnerFullControl,
  CannedACL'
  #-}
