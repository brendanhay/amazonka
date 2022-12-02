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
-- Module      : Amazonka.LexV2Models.Types.DialogActionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DialogActionType
  ( DialogActionType
      ( ..,
        DialogActionType_CloseIntent,
        DialogActionType_ConfirmIntent,
        DialogActionType_ElicitIntent,
        DialogActionType_ElicitSlot,
        DialogActionType_EndConversation,
        DialogActionType_EvaluateConditional,
        DialogActionType_FulfillIntent,
        DialogActionType_InvokeDialogCodeHook,
        DialogActionType_StartIntent
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DialogActionType = DialogActionType'
  { fromDialogActionType ::
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

pattern DialogActionType_CloseIntent :: DialogActionType
pattern DialogActionType_CloseIntent = DialogActionType' "CloseIntent"

pattern DialogActionType_ConfirmIntent :: DialogActionType
pattern DialogActionType_ConfirmIntent = DialogActionType' "ConfirmIntent"

pattern DialogActionType_ElicitIntent :: DialogActionType
pattern DialogActionType_ElicitIntent = DialogActionType' "ElicitIntent"

pattern DialogActionType_ElicitSlot :: DialogActionType
pattern DialogActionType_ElicitSlot = DialogActionType' "ElicitSlot"

pattern DialogActionType_EndConversation :: DialogActionType
pattern DialogActionType_EndConversation = DialogActionType' "EndConversation"

pattern DialogActionType_EvaluateConditional :: DialogActionType
pattern DialogActionType_EvaluateConditional = DialogActionType' "EvaluateConditional"

pattern DialogActionType_FulfillIntent :: DialogActionType
pattern DialogActionType_FulfillIntent = DialogActionType' "FulfillIntent"

pattern DialogActionType_InvokeDialogCodeHook :: DialogActionType
pattern DialogActionType_InvokeDialogCodeHook = DialogActionType' "InvokeDialogCodeHook"

pattern DialogActionType_StartIntent :: DialogActionType
pattern DialogActionType_StartIntent = DialogActionType' "StartIntent"

{-# COMPLETE
  DialogActionType_CloseIntent,
  DialogActionType_ConfirmIntent,
  DialogActionType_ElicitIntent,
  DialogActionType_ElicitSlot,
  DialogActionType_EndConversation,
  DialogActionType_EvaluateConditional,
  DialogActionType_FulfillIntent,
  DialogActionType_InvokeDialogCodeHook,
  DialogActionType_StartIntent,
  DialogActionType'
  #-}
