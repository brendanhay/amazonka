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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
  ( AccountTakeoverEventActionType
      ( ..,
        AccountTakeoverEventActionType_BLOCK,
        AccountTakeoverEventActionType_MFA_IF_CONFIGURED,
        AccountTakeoverEventActionType_MFA_REQUIRED,
        AccountTakeoverEventActionType_NO_ACTION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType'
  { fromAccountTakeoverEventActionType ::
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
