{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CannedAccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CannedAccessControlList
  ( CannedAccessControlList
    ( CannedAccessControlList'
    , CannedAccessControlListPrivate
    , CannedAccessControlListPublicRead
    , CannedAccessControlListPublicReadWrite
    , CannedAccessControlListAwsExecRead
    , CannedAccessControlListAuthenticatedRead
    , CannedAccessControlListBucketOwnerRead
    , CannedAccessControlListBucketOwnerFullControl
    , CannedAccessControlListLogDeliveryWrite
    , fromCannedAccessControlList
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CannedAccessControlList = CannedAccessControlList'{fromCannedAccessControlList
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern CannedAccessControlListPrivate :: CannedAccessControlList
pattern CannedAccessControlListPrivate = CannedAccessControlList' "private"

pattern CannedAccessControlListPublicRead :: CannedAccessControlList
pattern CannedAccessControlListPublicRead = CannedAccessControlList' "public-read"

pattern CannedAccessControlListPublicReadWrite :: CannedAccessControlList
pattern CannedAccessControlListPublicReadWrite = CannedAccessControlList' "public-read-write"

pattern CannedAccessControlListAwsExecRead :: CannedAccessControlList
pattern CannedAccessControlListAwsExecRead = CannedAccessControlList' "aws-exec-read"

pattern CannedAccessControlListAuthenticatedRead :: CannedAccessControlList
pattern CannedAccessControlListAuthenticatedRead = CannedAccessControlList' "authenticated-read"

pattern CannedAccessControlListBucketOwnerRead :: CannedAccessControlList
pattern CannedAccessControlListBucketOwnerRead = CannedAccessControlList' "bucket-owner-read"

pattern CannedAccessControlListBucketOwnerFullControl :: CannedAccessControlList
pattern CannedAccessControlListBucketOwnerFullControl = CannedAccessControlList' "bucket-owner-full-control"

pattern CannedAccessControlListLogDeliveryWrite :: CannedAccessControlList
pattern CannedAccessControlListLogDeliveryWrite = CannedAccessControlList' "log-delivery-write"

{-# COMPLETE 
  CannedAccessControlListPrivate,

  CannedAccessControlListPublicRead,

  CannedAccessControlListPublicReadWrite,

  CannedAccessControlListAwsExecRead,

  CannedAccessControlListAuthenticatedRead,

  CannedAccessControlListBucketOwnerRead,

  CannedAccessControlListBucketOwnerFullControl,

  CannedAccessControlListLogDeliveryWrite,
  CannedAccessControlList'
  #-}
