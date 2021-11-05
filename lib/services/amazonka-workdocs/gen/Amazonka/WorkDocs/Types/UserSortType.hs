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
-- Module      : Amazonka.WorkDocs.Types.UserSortType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UserSortType
  ( UserSortType
      ( ..,
        UserSortType_FULL_NAME,
        UserSortType_STORAGE_LIMIT,
        UserSortType_STORAGE_USED,
        UserSortType_USER_NAME,
        UserSortType_USER_STATUS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UserSortType = UserSortType'
  { fromUserSortType ::
      Core.Text
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

pattern UserSortType_FULL_NAME :: UserSortType
pattern UserSortType_FULL_NAME = UserSortType' "FULL_NAME"

pattern UserSortType_STORAGE_LIMIT :: UserSortType
pattern UserSortType_STORAGE_LIMIT = UserSortType' "STORAGE_LIMIT"

pattern UserSortType_STORAGE_USED :: UserSortType
pattern UserSortType_STORAGE_USED = UserSortType' "STORAGE_USED"

pattern UserSortType_USER_NAME :: UserSortType
pattern UserSortType_USER_NAME = UserSortType' "USER_NAME"

pattern UserSortType_USER_STATUS :: UserSortType
pattern UserSortType_USER_STATUS = UserSortType' "USER_STATUS"

{-# COMPLETE
  UserSortType_FULL_NAME,
  UserSortType_STORAGE_LIMIT,
  UserSortType_STORAGE_USED,
  UserSortType_USER_NAME,
  UserSortType_USER_STATUS,
  UserSortType'
  #-}
