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
-- Module      : Amazonka.ServiceCatalog.Types.AccessLevelFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.AccessLevelFilterKey
  ( AccessLevelFilterKey
      ( ..,
        AccessLevelFilterKey_Account,
        AccessLevelFilterKey_Role,
        AccessLevelFilterKey_User
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AccessLevelFilterKey = AccessLevelFilterKey'
  { fromAccessLevelFilterKey ::
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
