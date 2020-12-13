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
        Private,
        PublicRead,
        PublicReadWrite,
        AWSExecRead,
        AuthenticatedRead,
        BucketOwnerRead,
        BucketOwnerFullControl
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

pattern Private :: CannedACL
pattern Private = CannedACL' "private"

pattern PublicRead :: CannedACL
pattern PublicRead = CannedACL' "public-read"

pattern PublicReadWrite :: CannedACL
pattern PublicReadWrite = CannedACL' "public-read-write"

pattern AWSExecRead :: CannedACL
pattern AWSExecRead = CannedACL' "aws-exec-read"

pattern AuthenticatedRead :: CannedACL
pattern AuthenticatedRead = CannedACL' "authenticated-read"

pattern BucketOwnerRead :: CannedACL
pattern BucketOwnerRead = CannedACL' "bucket-owner-read"

pattern BucketOwnerFullControl :: CannedACL
pattern BucketOwnerFullControl = CannedACL' "bucket-owner-full-control"

{-# COMPLETE
  Private,
  PublicRead,
  PublicReadWrite,
  AWSExecRead,
  AuthenticatedRead,
  BucketOwnerRead,
  BucketOwnerFullControl,
  CannedACL'
  #-}
