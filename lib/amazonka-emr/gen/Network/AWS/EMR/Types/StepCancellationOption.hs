{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepCancellationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepCancellationOption where

import Network.AWS.Prelude

data StepCancellationOption
  = SendInterrupt
  | TerminateProcess
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

instance FromText StepCancellationOption where
  parser =
    takeLowerText >>= \case
      "send_interrupt" -> pure SendInterrupt
      "terminate_process" -> pure TerminateProcess
      e ->
        fromTextError $
          "Failure parsing StepCancellationOption from value: '" <> e
            <> "'. Accepted values: send_interrupt, terminate_process"

instance ToText StepCancellationOption where
  toText = \case
    SendInterrupt -> "SEND_INTERRUPT"
    TerminateProcess -> "TERMINATE_PROCESS"

instance Hashable StepCancellationOption

instance NFData StepCancellationOption

instance ToByteString StepCancellationOption

instance ToQuery StepCancellationOption

instance ToHeader StepCancellationOption

instance ToJSON StepCancellationOption where
  toJSON = toJSONText
