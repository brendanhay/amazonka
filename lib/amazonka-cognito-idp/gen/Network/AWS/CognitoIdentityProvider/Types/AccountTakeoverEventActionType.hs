{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
  ( AccountTakeoverEventActionType
    ( AccountTakeoverEventActionType'
    , AccountTakeoverEventActionTypeBlock
    , AccountTakeoverEventActionTypeMfaIfConfigured
    , AccountTakeoverEventActionTypeMfaRequired
    , AccountTakeoverEventActionTypeNoAction
    , fromAccountTakeoverEventActionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType'{fromAccountTakeoverEventActionType
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern AccountTakeoverEventActionTypeBlock :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionTypeBlock = AccountTakeoverEventActionType' "BLOCK"

pattern AccountTakeoverEventActionTypeMfaIfConfigured :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionTypeMfaIfConfigured = AccountTakeoverEventActionType' "MFA_IF_CONFIGURED"

pattern AccountTakeoverEventActionTypeMfaRequired :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionTypeMfaRequired = AccountTakeoverEventActionType' "MFA_REQUIRED"

pattern AccountTakeoverEventActionTypeNoAction :: AccountTakeoverEventActionType
pattern AccountTakeoverEventActionTypeNoAction = AccountTakeoverEventActionType' "NO_ACTION"

{-# COMPLETE 
  AccountTakeoverEventActionTypeBlock,

  AccountTakeoverEventActionTypeMfaIfConfigured,

  AccountTakeoverEventActionTypeMfaRequired,

  AccountTakeoverEventActionTypeNoAction,
  AccountTakeoverEventActionType'
  #-}
