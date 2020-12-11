-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserSortType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserSortType
  ( UserSortType
      ( UserSortType',
        FullName,
        StorageLimit,
        StorageUsed,
        UserName,
        UserStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserSortType = UserSortType' Lude.Text
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

pattern FullName :: UserSortType
pattern FullName = UserSortType' "FULL_NAME"

pattern StorageLimit :: UserSortType
pattern StorageLimit = UserSortType' "STORAGE_LIMIT"

pattern StorageUsed :: UserSortType
pattern StorageUsed = UserSortType' "STORAGE_USED"

pattern UserName :: UserSortType
pattern UserName = UserSortType' "USER_NAME"

pattern UserStatus :: UserSortType
pattern UserStatus = UserSortType' "USER_STATUS"

{-# COMPLETE
  FullName,
  StorageLimit,
  StorageUsed,
  UserName,
  UserStatus,
  UserSortType'
  #-}
