{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Permission
  ( Permission
    ( Permission'
    , PermissionAll
    , PermissionSelect
    , PermissionAlter
    , PermissionDrop
    , PermissionDelete
    , PermissionInsert
    , PermissionCreateDatabase
    , PermissionCreateTable
    , PermissionDataLocationAccess
    , fromPermission
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Permission = Permission'{fromPermission :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern PermissionAll :: Permission
pattern PermissionAll = Permission' "ALL"

pattern PermissionSelect :: Permission
pattern PermissionSelect = Permission' "SELECT"

pattern PermissionAlter :: Permission
pattern PermissionAlter = Permission' "ALTER"

pattern PermissionDrop :: Permission
pattern PermissionDrop = Permission' "DROP"

pattern PermissionDelete :: Permission
pattern PermissionDelete = Permission' "DELETE"

pattern PermissionInsert :: Permission
pattern PermissionInsert = Permission' "INSERT"

pattern PermissionCreateDatabase :: Permission
pattern PermissionCreateDatabase = Permission' "CREATE_DATABASE"

pattern PermissionCreateTable :: Permission
pattern PermissionCreateTable = Permission' "CREATE_TABLE"

pattern PermissionDataLocationAccess :: Permission
pattern PermissionDataLocationAccess = Permission' "DATA_LOCATION_ACCESS"

{-# COMPLETE 
  PermissionAll,

  PermissionSelect,

  PermissionAlter,

  PermissionDrop,

  PermissionDelete,

  PermissionInsert,

  PermissionCreateDatabase,

  PermissionCreateTable,

  PermissionDataLocationAccess,
  Permission'
  #-}
