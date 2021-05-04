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

import qualified Network.AWS.Prelude as Prelude

newtype AccessLevelFilterKey = AccessLevelFilterKey'
  { fromAccessLevelFilterKey ::
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
