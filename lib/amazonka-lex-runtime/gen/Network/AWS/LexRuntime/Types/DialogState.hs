-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.DialogState
  ( DialogState
      ( DialogState',
        ConfirmIntent,
        ElicitIntent,
        ElicitSlot,
        Failed,
        Fulfilled,
        ReadyForFulfillment
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DialogState = DialogState' Lude.Text
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

pattern ConfirmIntent :: DialogState
pattern ConfirmIntent = DialogState' "ConfirmIntent"

pattern ElicitIntent :: DialogState
pattern ElicitIntent = DialogState' "ElicitIntent"

pattern ElicitSlot :: DialogState
pattern ElicitSlot = DialogState' "ElicitSlot"

pattern Failed :: DialogState
pattern Failed = DialogState' "Failed"

pattern Fulfilled :: DialogState
pattern Fulfilled = DialogState' "Fulfilled"

pattern ReadyForFulfillment :: DialogState
pattern ReadyForFulfillment = DialogState' "ReadyForFulfillment"

{-# COMPLETE
  ConfirmIntent,
  ElicitIntent,
  ElicitSlot,
  Failed,
  Fulfilled,
  ReadyForFulfillment,
  DialogState'
  #-}
