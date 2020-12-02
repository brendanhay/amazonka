{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SpotInstanceState
  = SISActive
  | SISCancelled
  | SISClosed
  | SISFailed
  | SISOpen
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

instance FromText SpotInstanceState where
  parser =
    takeLowerText >>= \case
      "active" -> pure SISActive
      "cancelled" -> pure SISCancelled
      "closed" -> pure SISClosed
      "failed" -> pure SISFailed
      "open" -> pure SISOpen
      e ->
        fromTextError $
          "Failure parsing SpotInstanceState from value: '" <> e
            <> "'. Accepted values: active, cancelled, closed, failed, open"

instance ToText SpotInstanceState where
  toText = \case
    SISActive -> "active"
    SISCancelled -> "cancelled"
    SISClosed -> "closed"
    SISFailed -> "failed"
    SISOpen -> "open"

instance Hashable SpotInstanceState

instance NFData SpotInstanceState

instance ToByteString SpotInstanceState

instance ToQuery SpotInstanceState

instance ToHeader SpotInstanceState

instance FromXML SpotInstanceState where
  parseXML = parseXMLText "SpotInstanceState"
