-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Permission
  ( Permission
      ( Permission',
        PAll,
        PAlter,
        PCreateDatabase,
        PCreateTable,
        PDataLocationAccess,
        PDelete,
        PDrop,
        PInsert,
        PSelect
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Permission = Permission' Lude.Text
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

pattern PAll :: Permission
pattern PAll = Permission' "ALL"

pattern PAlter :: Permission
pattern PAlter = Permission' "ALTER"

pattern PCreateDatabase :: Permission
pattern PCreateDatabase = Permission' "CREATE_DATABASE"

pattern PCreateTable :: Permission
pattern PCreateTable = Permission' "CREATE_TABLE"

pattern PDataLocationAccess :: Permission
pattern PDataLocationAccess = Permission' "DATA_LOCATION_ACCESS"

pattern PDelete :: Permission
pattern PDelete = Permission' "DELETE"

pattern PDrop :: Permission
pattern PDrop = Permission' "DROP"

pattern PInsert :: Permission
pattern PInsert = Permission' "INSERT"

pattern PSelect :: Permission
pattern PSelect = Permission' "SELECT"

{-# COMPLETE
  PAll,
  PAlter,
  PCreateDatabase,
  PCreateTable,
  PDataLocationAccess,
  PDelete,
  PDrop,
  PInsert,
  PSelect,
  Permission'
  #-}
