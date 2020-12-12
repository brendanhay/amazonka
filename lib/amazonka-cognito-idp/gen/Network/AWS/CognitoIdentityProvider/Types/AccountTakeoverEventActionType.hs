{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
  ( AccountTakeoverEventActionType
      ( AccountTakeoverEventActionType',
        ATEATBlock,
        ATEATMFAIfConfigured,
        ATEATMFARequired,
        ATEATNoAction
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AccountTakeoverEventActionType = AccountTakeoverEventActionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ATEATBlock :: AccountTakeoverEventActionType
pattern ATEATBlock = AccountTakeoverEventActionType' "BLOCK"

pattern ATEATMFAIfConfigured :: AccountTakeoverEventActionType
pattern ATEATMFAIfConfigured = AccountTakeoverEventActionType' "MFA_IF_CONFIGURED"

pattern ATEATMFARequired :: AccountTakeoverEventActionType
pattern ATEATMFARequired = AccountTakeoverEventActionType' "MFA_REQUIRED"

pattern ATEATNoAction :: AccountTakeoverEventActionType
pattern ATEATNoAction = AccountTakeoverEventActionType' "NO_ACTION"

{-# COMPLETE
  ATEATBlock,
  ATEATMFAIfConfigured,
  ATEATMFARequired,
  ATEATNoAction,
  AccountTakeoverEventActionType'
  #-}
