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
-- Module      : Amazonka.LexRuntime.Types.DialogActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.DialogActionType
  ( DialogActionType
      ( ..,
        DialogActionType_Close,
        DialogActionType_ConfirmIntent,
        DialogActionType_Delegate,
        DialogActionType_ElicitIntent,
        DialogActionType_ElicitSlot
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

pattern DialogActionType_Close :: DialogActionType
pattern DialogActionType_Close = DialogActionType' "Close"

pattern DialogActionType_ConfirmIntent :: DialogActionType
pattern DialogActionType_ConfirmIntent = DialogActionType' "ConfirmIntent"

pattern DialogActionType_Delegate :: DialogActionType
pattern DialogActionType_Delegate = DialogActionType' "Delegate"

pattern DialogActionType_ElicitIntent :: DialogActionType
pattern DialogActionType_ElicitIntent = DialogActionType' "ElicitIntent"

pattern DialogActionType_ElicitSlot :: DialogActionType
pattern DialogActionType_ElicitSlot = DialogActionType' "ElicitSlot"

{-# COMPLETE
  DialogActionType_Close,
  DialogActionType_ConfirmIntent,
  DialogActionType_Delegate,
  DialogActionType_ElicitIntent,
  DialogActionType_ElicitSlot,
  DialogActionType'
  #-}
