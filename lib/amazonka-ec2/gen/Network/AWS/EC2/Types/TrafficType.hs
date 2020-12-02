{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficType
  = TTAccept
  | TTAll
  | TTReject
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

instance FromText TrafficType where
  parser =
    takeLowerText >>= \case
      "accept" -> pure TTAccept
      "all" -> pure TTAll
      "reject" -> pure TTReject
      e ->
        fromTextError $
          "Failure parsing TrafficType from value: '" <> e
            <> "'. Accepted values: accept, all, reject"

instance ToText TrafficType where
  toText = \case
    TTAccept -> "ACCEPT"
    TTAll -> "ALL"
    TTReject -> "REJECT"

instance Hashable TrafficType

instance NFData TrafficType

instance ToByteString TrafficType

instance ToQuery TrafficType

instance ToHeader TrafficType

instance FromXML TrafficType where
  parseXML = parseXMLText "TrafficType"
