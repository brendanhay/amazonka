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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccessLevelFilterKey = AccessLevelFilterKey'
  { fromAccessLevelFilterKey ::
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
