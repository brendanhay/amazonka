{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import qualified Network.AWS.Core as Core

newtype Permission = Permission'
  { fromPermission ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
