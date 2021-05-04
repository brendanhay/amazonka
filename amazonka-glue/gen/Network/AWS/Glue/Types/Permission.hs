{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Permission
  ( Permission
      ( ..,
        Permission_ALL,
        Permission_ALTER,
        Permission_CREATE_DATABASE,
        Permission_CREATE_TABLE,
        Permission_DATA_LOCATION_ACCESS,
        Permission_DELETE,
        Permission_DROP,
        Permission_INSERT,
        Permission_SELECT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype Permission = Permission'
  { fromPermission ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern Permission_ALL :: Permission
pattern Permission_ALL = Permission' "ALL"

pattern Permission_ALTER :: Permission
pattern Permission_ALTER = Permission' "ALTER"

pattern Permission_CREATE_DATABASE :: Permission
pattern Permission_CREATE_DATABASE = Permission' "CREATE_DATABASE"

pattern Permission_CREATE_TABLE :: Permission
pattern Permission_CREATE_TABLE = Permission' "CREATE_TABLE"

pattern Permission_DATA_LOCATION_ACCESS :: Permission
pattern Permission_DATA_LOCATION_ACCESS = Permission' "DATA_LOCATION_ACCESS"

pattern Permission_DELETE :: Permission
pattern Permission_DELETE = Permission' "DELETE"

pattern Permission_DROP :: Permission
pattern Permission_DROP = Permission' "DROP"

pattern Permission_INSERT :: Permission
pattern Permission_INSERT = Permission' "INSERT"

pattern Permission_SELECT :: Permission
pattern Permission_SELECT = Permission' "SELECT"

{-# COMPLETE
  Permission_ALL,
  Permission_ALTER,
  Permission_CREATE_DATABASE,
  Permission_CREATE_TABLE,
  Permission_DATA_LOCATION_ACCESS,
  Permission_DELETE,
  Permission_DROP,
  Permission_INSERT,
  Permission_SELECT,
  Permission'
  #-}
