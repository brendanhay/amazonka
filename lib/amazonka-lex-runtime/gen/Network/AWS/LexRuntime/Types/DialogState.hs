{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        DialogStateElicitIntent,
        DialogStateConfirmIntent,
        DialogStateElicitSlot,
        DialogStateFulfilled,
        DialogStateReadyForFulfillment,
        DialogStateFailed,
        fromDialogState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DialogState = DialogState' {fromDialogState :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DialogStateElicitIntent :: DialogState
pattern DialogStateElicitIntent = DialogState' "ElicitIntent"

pattern DialogStateConfirmIntent :: DialogState
pattern DialogStateConfirmIntent = DialogState' "ConfirmIntent"

pattern DialogStateElicitSlot :: DialogState
pattern DialogStateElicitSlot = DialogState' "ElicitSlot"

pattern DialogStateFulfilled :: DialogState
pattern DialogStateFulfilled = DialogState' "Fulfilled"

pattern DialogStateReadyForFulfillment :: DialogState
pattern DialogStateReadyForFulfillment = DialogState' "ReadyForFulfillment"

pattern DialogStateFailed :: DialogState
pattern DialogStateFailed = DialogState' "Failed"

{-# COMPLETE
  DialogStateElicitIntent,
  DialogStateConfirmIntent,
  DialogStateElicitSlot,
  DialogStateFulfilled,
  DialogStateReadyForFulfillment,
  DialogStateFailed,
  DialogState'
  #-}
