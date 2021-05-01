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
-- Module      : Network.AWS.Glacier.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Permission
  ( Permission
      ( ..,
        Permission_FULL_CONTROL,
        Permission_READ,
        Permission_READ_ACP,
        Permission_WRITE,
        Permission_WRITE_ACP
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

pattern Permission_FULL_CONTROL :: Permission
pattern Permission_FULL_CONTROL = Permission' "FULL_CONTROL"

pattern Permission_READ :: Permission
pattern Permission_READ = Permission' "READ"

pattern Permission_READ_ACP :: Permission
pattern Permission_READ_ACP = Permission' "READ_ACP"

pattern Permission_WRITE :: Permission
pattern Permission_WRITE = Permission' "WRITE"

pattern Permission_WRITE_ACP :: Permission
pattern Permission_WRITE_ACP = Permission' "WRITE_ACP"

{-# COMPLETE
  Permission_FULL_CONTROL,
  Permission_READ,
  Permission_READ_ACP,
  Permission_WRITE,
  Permission_WRITE_ACP,
  Permission'
  #-}
