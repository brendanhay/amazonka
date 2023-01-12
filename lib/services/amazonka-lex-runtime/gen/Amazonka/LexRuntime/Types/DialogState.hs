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
-- Module      : Amazonka.LexRuntime.Types.DialogState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.DialogState
  ( DialogState
      ( ..,
        DialogState_ConfirmIntent,
        DialogState_ElicitIntent,
        DialogState_ElicitSlot,
        DialogState_Failed,
        DialogState_Fulfilled,
        DialogState_ReadyForFulfillment
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DialogState = DialogState'
  { fromDialogState ::
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

pattern DialogState_ConfirmIntent :: DialogState
pattern DialogState_ConfirmIntent = DialogState' "ConfirmIntent"

pattern DialogState_ElicitIntent :: DialogState
pattern DialogState_ElicitIntent = DialogState' "ElicitIntent"

pattern DialogState_ElicitSlot :: DialogState
pattern DialogState_ElicitSlot = DialogState' "ElicitSlot"

pattern DialogState_Failed :: DialogState
pattern DialogState_Failed = DialogState' "Failed"

pattern DialogState_Fulfilled :: DialogState
pattern DialogState_Fulfilled = DialogState' "Fulfilled"

pattern DialogState_ReadyForFulfillment :: DialogState
pattern DialogState_ReadyForFulfillment = DialogState' "ReadyForFulfillment"

{-# COMPLETE
  DialogState_ConfirmIntent,
  DialogState_ElicitIntent,
  DialogState_ElicitSlot,
  DialogState_Failed,
  DialogState_Fulfilled,
  DialogState_ReadyForFulfillment,
  DialogState'
  #-}
