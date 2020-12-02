{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetEventType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetEventType
  = FETFleetChange
  | FETInstanceChange
  | FETServiceError
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

instance FromText FleetEventType where
  parser =
    takeLowerText >>= \case
      "fleet-change" -> pure FETFleetChange
      "instance-change" -> pure FETInstanceChange
      "service-error" -> pure FETServiceError
      e ->
        fromTextError $
          "Failure parsing FleetEventType from value: '" <> e
            <> "'. Accepted values: fleet-change, instance-change, service-error"

instance ToText FleetEventType where
  toText = \case
    FETFleetChange -> "fleet-change"
    FETInstanceChange -> "instance-change"
    FETServiceError -> "service-error"

instance Hashable FleetEventType

instance NFData FleetEventType

instance ToByteString FleetEventType

instance ToQuery FleetEventType

instance ToHeader FleetEventType

instance FromXML FleetEventType where
  parseXML = parseXMLText "FleetEventType"
