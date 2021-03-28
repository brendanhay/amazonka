{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserSortType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UserSortType
  ( UserSortType
    ( UserSortType'
    , UserSortTypeUserName
    , UserSortTypeFullName
    , UserSortTypeStorageLimit
    , UserSortTypeUserStatus
    , UserSortTypeStorageUsed
    , fromUserSortType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UserSortType = UserSortType'{fromUserSortType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern UserSortTypeUserName :: UserSortType
pattern UserSortTypeUserName = UserSortType' "USER_NAME"

pattern UserSortTypeFullName :: UserSortType
pattern UserSortTypeFullName = UserSortType' "FULL_NAME"

pattern UserSortTypeStorageLimit :: UserSortType
pattern UserSortTypeStorageLimit = UserSortType' "STORAGE_LIMIT"

pattern UserSortTypeUserStatus :: UserSortType
pattern UserSortTypeUserStatus = UserSortType' "USER_STATUS"

pattern UserSortTypeStorageUsed :: UserSortType
pattern UserSortTypeStorageUsed = UserSortType' "STORAGE_USED"

{-# COMPLETE 
  UserSortTypeUserName,

  UserSortTypeFullName,

  UserSortTypeStorageLimit,

  UserSortTypeUserStatus,

  UserSortTypeStorageUsed,
  UserSortType'
  #-}
