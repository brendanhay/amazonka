{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionStatus where

import Network.AWS.Prelude

data GameSessionStatus
  = GSSActivating
  | GSSActive
  | GSSError'
  | GSSTerminated
  | GSSTerminating
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

instance FromText GameSessionStatus where
  parser =
    takeLowerText >>= \case
      "activating" -> pure GSSActivating
      "active" -> pure GSSActive
      "error" -> pure GSSError'
      "terminated" -> pure GSSTerminated
      "terminating" -> pure GSSTerminating
      e ->
        fromTextError $
          "Failure parsing GameSessionStatus from value: '" <> e
            <> "'. Accepted values: activating, active, error, terminated, terminating"

instance ToText GameSessionStatus where
  toText = \case
    GSSActivating -> "ACTIVATING"
    GSSActive -> "ACTIVE"
    GSSError' -> "ERROR"
    GSSTerminated -> "TERMINATED"
    GSSTerminating -> "TERMINATING"

instance Hashable GameSessionStatus

instance NFData GameSessionStatus

instance ToByteString GameSessionStatus

instance ToQuery GameSessionStatus

instance ToHeader GameSessionStatus

instance FromJSON GameSessionStatus where
  parseJSON = parseJSONText "GameSessionStatus"
