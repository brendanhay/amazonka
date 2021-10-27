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
-- Module      : Network.AWS.LexV2Runtime.Types.IntentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.IntentState
  ( IntentState
      ( ..,
        IntentState_Failed,
        IntentState_Fulfilled,
        IntentState_FulfillmentInProgress,
        IntentState_InProgress,
        IntentState_ReadyForFulfillment,
        IntentState_Waiting
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype IntentState = IntentState'
  { fromIntentState ::
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

pattern IntentState_Failed :: IntentState
pattern IntentState_Failed = IntentState' "Failed"

pattern IntentState_Fulfilled :: IntentState
pattern IntentState_Fulfilled = IntentState' "Fulfilled"

pattern IntentState_FulfillmentInProgress :: IntentState
pattern IntentState_FulfillmentInProgress = IntentState' "FulfillmentInProgress"

pattern IntentState_InProgress :: IntentState
pattern IntentState_InProgress = IntentState' "InProgress"

pattern IntentState_ReadyForFulfillment :: IntentState
pattern IntentState_ReadyForFulfillment = IntentState' "ReadyForFulfillment"

pattern IntentState_Waiting :: IntentState
pattern IntentState_Waiting = IntentState' "Waiting"

{-# COMPLETE
  IntentState_Failed,
  IntentState_Fulfilled,
  IntentState_FulfillmentInProgress,
  IntentState_InProgress,
  IntentState_ReadyForFulfillment,
  IntentState_Waiting,
  IntentState'
  #-}
