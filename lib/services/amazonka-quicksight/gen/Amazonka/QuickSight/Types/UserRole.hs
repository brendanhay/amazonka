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
-- Module      : Amazonka.QuickSight.Types.UserRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UserRole
  ( UserRole
      ( ..,
        UserRole_ADMIN,
        UserRole_AUTHOR,
        UserRole_READER,
        UserRole_RESTRICTED_AUTHOR,
        UserRole_RESTRICTED_READER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UserRole = UserRole'
  { fromUserRole ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern UserRole_ADMIN :: UserRole
pattern UserRole_ADMIN = UserRole' "ADMIN"

pattern UserRole_AUTHOR :: UserRole
pattern UserRole_AUTHOR = UserRole' "AUTHOR"

pattern UserRole_READER :: UserRole
pattern UserRole_READER = UserRole' "READER"

pattern UserRole_RESTRICTED_AUTHOR :: UserRole
pattern UserRole_RESTRICTED_AUTHOR = UserRole' "RESTRICTED_AUTHOR"

pattern UserRole_RESTRICTED_READER :: UserRole
pattern UserRole_RESTRICTED_READER = UserRole' "RESTRICTED_READER"

{-# COMPLETE
  UserRole_ADMIN,
  UserRole_AUTHOR,
  UserRole_READER,
  UserRole_RESTRICTED_AUTHOR,
  UserRole_RESTRICTED_READER,
  UserRole'
  #-}
