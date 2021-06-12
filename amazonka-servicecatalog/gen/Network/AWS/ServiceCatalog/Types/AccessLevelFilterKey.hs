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
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
  ( AccessLevelFilterKey
      ( ..,
        AccessLevelFilterKey_Account,
        AccessLevelFilterKey_Role,
        AccessLevelFilterKey_User
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AccessLevelFilterKey = AccessLevelFilterKey'
  { fromAccessLevelFilterKey ::
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

pattern AccessLevelFilterKey_Account :: AccessLevelFilterKey
pattern AccessLevelFilterKey_Account = AccessLevelFilterKey' "Account"

pattern AccessLevelFilterKey_Role :: AccessLevelFilterKey
pattern AccessLevelFilterKey_Role = AccessLevelFilterKey' "Role"

pattern AccessLevelFilterKey_User :: AccessLevelFilterKey
pattern AccessLevelFilterKey_User = AccessLevelFilterKey' "User"

{-# COMPLETE
  AccessLevelFilterKey_Account,
  AccessLevelFilterKey_Role,
  AccessLevelFilterKey_User,
  AccessLevelFilterKey'
  #-}
