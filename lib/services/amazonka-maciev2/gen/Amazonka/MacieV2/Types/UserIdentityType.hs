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
-- Module      : Amazonka.MacieV2.Types.UserIdentityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UserIdentityType
  ( UserIdentityType
      ( ..,
        UserIdentityType_AWSAccount,
        UserIdentityType_AWSService,
        UserIdentityType_AssumedRole,
        UserIdentityType_FederatedUser,
        UserIdentityType_IAMUser,
        UserIdentityType_Root
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of entity that performed the action on the affected resource.
-- Possible values are:
newtype UserIdentityType = UserIdentityType'
  { fromUserIdentityType ::
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

pattern UserIdentityType_AWSAccount :: UserIdentityType
pattern UserIdentityType_AWSAccount = UserIdentityType' "AWSAccount"

pattern UserIdentityType_AWSService :: UserIdentityType
pattern UserIdentityType_AWSService = UserIdentityType' "AWSService"

pattern UserIdentityType_AssumedRole :: UserIdentityType
pattern UserIdentityType_AssumedRole = UserIdentityType' "AssumedRole"

pattern UserIdentityType_FederatedUser :: UserIdentityType
pattern UserIdentityType_FederatedUser = UserIdentityType' "FederatedUser"

pattern UserIdentityType_IAMUser :: UserIdentityType
pattern UserIdentityType_IAMUser = UserIdentityType' "IAMUser"

pattern UserIdentityType_Root :: UserIdentityType
pattern UserIdentityType_Root = UserIdentityType' "Root"

{-# COMPLETE
  UserIdentityType_AWSAccount,
  UserIdentityType_AWSService,
  UserIdentityType_AssumedRole,
  UserIdentityType_FederatedUser,
  UserIdentityType_IAMUser,
  UserIdentityType_Root,
  UserIdentityType'
  #-}
