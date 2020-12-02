{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EventType
  = ETError'
  | ETFleetRequestChange
  | ETInformation
  | ETInstanceChange
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

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "error" -> pure ETError'
      "fleetrequestchange" -> pure ETFleetRequestChange
      "information" -> pure ETInformation
      "instancechange" -> pure ETInstanceChange
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: error, fleetrequestchange, information, instancechange"

instance ToText EventType where
  toText = \case
    ETError' -> "error"
    ETFleetRequestChange -> "fleetRequestChange"
    ETInformation -> "information"
    ETInstanceChange -> "instanceChange"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance FromXML EventType where
  parseXML = parseXMLText "EventType"
