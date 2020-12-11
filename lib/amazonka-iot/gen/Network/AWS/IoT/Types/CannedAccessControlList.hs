-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CannedAccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CannedAccessControlList
  ( CannedAccessControlList
      ( CannedAccessControlList',
        AWSExecRead,
        AuthenticatedRead,
        BucketOwnerFullControl,
        BucketOwnerRead,
        LogDeliveryWrite,
        Private,
        PublicRead,
        PublicReadWrite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CannedAccessControlList = CannedAccessControlList' Lude.Text
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

pattern AWSExecRead :: CannedAccessControlList
pattern AWSExecRead = CannedAccessControlList' "aws-exec-read"

pattern AuthenticatedRead :: CannedAccessControlList
pattern AuthenticatedRead = CannedAccessControlList' "authenticated-read"

pattern BucketOwnerFullControl :: CannedAccessControlList
pattern BucketOwnerFullControl = CannedAccessControlList' "bucket-owner-full-control"

pattern BucketOwnerRead :: CannedAccessControlList
pattern BucketOwnerRead = CannedAccessControlList' "bucket-owner-read"

pattern LogDeliveryWrite :: CannedAccessControlList
pattern LogDeliveryWrite = CannedAccessControlList' "log-delivery-write"

pattern Private :: CannedAccessControlList
pattern Private = CannedAccessControlList' "private"

pattern PublicRead :: CannedAccessControlList
pattern PublicRead = CannedAccessControlList' "public-read"

pattern PublicReadWrite :: CannedAccessControlList
pattern PublicReadWrite = CannedAccessControlList' "public-read-write"

{-# COMPLETE
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  LogDeliveryWrite,
  Private,
  PublicRead,
  PublicReadWrite,
  CannedAccessControlList'
  #-}
