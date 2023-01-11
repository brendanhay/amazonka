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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
  ( AccountTakeoverEventActionType
      ( ..,
        AccountTakeoverEventActionType_BLOCK,
        AccountTakeoverEventActionType_MFA_IF_CONFIGURED,
        AccountTakeoverEventActionType_MFA_REQUIRED,
        AccountTakeoverEventActionType_NO_ACTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType'
  { fromAccountTakeoverEventActionType ::
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

pattern AccountTakeoverEventActionType_BLOCK :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionType_BLOCK = AccountTakeoverEventActionType' "BLOCK"

pattern AccountTakeoverEventActionType_MFA_IF_CONFIGURED :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionType_MFA_IF_CONFIGURED = AccountTakeoverEventActionType' "MFA_IF_CONFIGURED"

pattern AccountTakeoverEventActionType_MFA_REQUIRED :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionType_MFA_REQUIRED = AccountTakeoverEventActionType' "MFA_REQUIRED"

pattern AccountTakeoverEventActionType_NO_ACTION :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionType_NO_ACTION = AccountTakeoverEventActionType' "NO_ACTION"

{-# COMPLETE
  AccountTakeoverEventActionType_BLOCK,
  AccountTakeoverEventActionType_MFA_IF_CONFIGURED,
  AccountTakeoverEventActionType_MFA_REQUIRED,
  AccountTakeoverEventActionType_NO_ACTION,
  AccountTakeoverEventActionType'
  #-}
