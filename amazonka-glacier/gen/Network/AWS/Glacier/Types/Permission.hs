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
