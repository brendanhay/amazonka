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
        AWSExecRead,
        AuthenticatedRead,
        BucketOwnerFullControl,
        BucketOwnerRead,
        Private,
        PublicRead,
        PublicReadWrite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CannedACL = CannedACL' Lude.Text
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

pattern AWSExecRead :: CannedACL
pattern AWSExecRead = CannedACL' "aws-exec-read"

pattern AuthenticatedRead :: CannedACL
pattern AuthenticatedRead = CannedACL' "authenticated-read"

pattern BucketOwnerFullControl :: CannedACL
pattern BucketOwnerFullControl = CannedACL' "bucket-owner-full-control"

pattern BucketOwnerRead :: CannedACL
pattern BucketOwnerRead = CannedACL' "bucket-owner-read"

pattern Private :: CannedACL
pattern Private = CannedACL' "private"

pattern PublicRead :: CannedACL
pattern PublicRead = CannedACL' "public-read"

pattern PublicReadWrite :: CannedACL
pattern PublicReadWrite = CannedACL' "public-read-write"

{-# COMPLETE
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  Private,
  PublicRead,
  PublicReadWrite,
  CannedACL'
  #-}
