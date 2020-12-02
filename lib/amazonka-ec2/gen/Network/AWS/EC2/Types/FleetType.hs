{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetType
  = FTInstant
  | FTMaintain
  | FTRequest
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

instance FromText FleetType where
  parser =
    takeLowerText >>= \case
      "instant" -> pure FTInstant
      "maintain" -> pure FTMaintain
      "request" -> pure FTRequest
      e ->
        fromTextError $
          "Failure parsing FleetType from value: '" <> e
            <> "'. Accepted values: instant, maintain, request"

instance ToText FleetType where
  toText = \case
    FTInstant -> "instant"
    FTMaintain -> "maintain"
    FTRequest -> "request"

instance Hashable FleetType

instance NFData FleetType

instance ToByteString FleetType

instance ToQuery FleetType

instance ToHeader FleetType

instance FromXML FleetType where
  parseXML = parseXMLText "FleetType"
