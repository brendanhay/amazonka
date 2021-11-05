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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType'
  { fromAccountTakeoverEventActionType ::
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
