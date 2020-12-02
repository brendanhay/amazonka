{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelTimerFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CancelTimerFailedCause where

import Network.AWS.Prelude

data CancelTimerFailedCause
  = CTFCOperationNotPermitted
  | CTFCTimerIdUnknown
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CancelTimerFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure CTFCOperationNotPermitted
      "timer_id_unknown" -> pure CTFCTimerIdUnknown
      e ->
        fromTextError $
          "Failure parsing CancelTimerFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted, timer_id_unknown"

instance ToText CancelTimerFailedCause where
  toText = \case
    CTFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    CTFCTimerIdUnknown -> "TIMER_ID_UNKNOWN"

instance Hashable CancelTimerFailedCause

instance NFData CancelTimerFailedCause

instance ToByteString CancelTimerFailedCause

instance ToQuery CancelTimerFailedCause

instance ToHeader CancelTimerFailedCause

instance FromJSON CancelTimerFailedCause where
  parseJSON = parseJSONText "CancelTimerFailedCause"
