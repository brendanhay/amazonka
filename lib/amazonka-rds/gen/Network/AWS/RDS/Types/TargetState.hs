{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetState where

import Network.AWS.Prelude

data TargetState
  = TSAvailable
  | TSRegistering
  | TSUnavailable
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

instance FromText TargetState where
  parser =
    takeLowerText >>= \case
      "available" -> pure TSAvailable
      "registering" -> pure TSRegistering
      "unavailable" -> pure TSUnavailable
      e ->
        fromTextError $
          "Failure parsing TargetState from value: '" <> e
            <> "'. Accepted values: available, registering, unavailable"

instance ToText TargetState where
  toText = \case
    TSAvailable -> "AVAILABLE"
    TSRegistering -> "REGISTERING"
    TSUnavailable -> "UNAVAILABLE"

instance Hashable TargetState

instance NFData TargetState

instance ToByteString TargetState

instance ToQuery TargetState

instance ToHeader TargetState

instance FromXML TargetState where
  parseXML = parseXMLText "TargetState"
