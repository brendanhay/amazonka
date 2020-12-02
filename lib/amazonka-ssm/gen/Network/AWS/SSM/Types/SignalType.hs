{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SignalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SignalType where

import Network.AWS.Prelude

data SignalType
  = Approve
  | Reject
  | Resume
  | StartStep
  | StopStep
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

instance FromText SignalType where
  parser =
    takeLowerText >>= \case
      "approve" -> pure Approve
      "reject" -> pure Reject
      "resume" -> pure Resume
      "startstep" -> pure StartStep
      "stopstep" -> pure StopStep
      e ->
        fromTextError $
          "Failure parsing SignalType from value: '" <> e
            <> "'. Accepted values: approve, reject, resume, startstep, stopstep"

instance ToText SignalType where
  toText = \case
    Approve -> "Approve"
    Reject -> "Reject"
    Resume -> "Resume"
    StartStep -> "StartStep"
    StopStep -> "StopStep"

instance Hashable SignalType

instance NFData SignalType

instance ToByteString SignalType

instance ToQuery SignalType

instance ToHeader SignalType

instance ToJSON SignalType where
  toJSON = toJSONText
