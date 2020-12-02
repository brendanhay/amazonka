{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartTimerFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartTimerFailedCause where

import Network.AWS.Prelude

data StartTimerFailedCause
  = STFCOpenTimersLimitExceeded
  | STFCOperationNotPermitted
  | STFCTimerCreationRateExceeded
  | STFCTimerIdAlreadyInUse
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

instance FromText StartTimerFailedCause where
  parser =
    takeLowerText >>= \case
      "open_timers_limit_exceeded" -> pure STFCOpenTimersLimitExceeded
      "operation_not_permitted" -> pure STFCOperationNotPermitted
      "timer_creation_rate_exceeded" -> pure STFCTimerCreationRateExceeded
      "timer_id_already_in_use" -> pure STFCTimerIdAlreadyInUse
      e ->
        fromTextError $
          "Failure parsing StartTimerFailedCause from value: '" <> e
            <> "'. Accepted values: open_timers_limit_exceeded, operation_not_permitted, timer_creation_rate_exceeded, timer_id_already_in_use"

instance ToText StartTimerFailedCause where
  toText = \case
    STFCOpenTimersLimitExceeded -> "OPEN_TIMERS_LIMIT_EXCEEDED"
    STFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
    STFCTimerCreationRateExceeded -> "TIMER_CREATION_RATE_EXCEEDED"
    STFCTimerIdAlreadyInUse -> "TIMER_ID_ALREADY_IN_USE"

instance Hashable StartTimerFailedCause

instance NFData StartTimerFailedCause

instance ToByteString StartTimerFailedCause

instance ToQuery StartTimerFailedCause

instance ToHeader StartTimerFailedCause

instance FromJSON StartTimerFailedCause where
  parseJSON = parseJSONText "StartTimerFailedCause"
