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
-- Module      : Network.AWS.WorkDocs.Types.UserStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserStatusType
  ( UserStatusType
      ( ..,
        UserStatusType_ACTIVE,
        UserStatusType_INACTIVE,
        UserStatusType_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UserStatusType = UserStatusType'
  { fromUserStatusType ::
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

pattern UserStatusType_ACTIVE :: UserStatusType
pattern UserStatusType_ACTIVE = UserStatusType' "ACTIVE"

pattern UserStatusType_INACTIVE :: UserStatusType
pattern UserStatusType_INACTIVE = UserStatusType' "INACTIVE"

pattern UserStatusType_PENDING :: UserStatusType
pattern UserStatusType_PENDING = UserStatusType' "PENDING"

{-# COMPLETE
  UserStatusType_ACTIVE,
  UserStatusType_INACTIVE,
  UserStatusType_PENDING,
  UserStatusType'
  #-}
