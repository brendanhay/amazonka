{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DatafeedSubscriptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DatafeedSubscriptionState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DatafeedSubscriptionState
  = DSSActive
  | DSSInactive
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

instance FromText DatafeedSubscriptionState where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSSActive
      "inactive" -> pure DSSInactive
      e ->
        fromTextError $
          "Failure parsing DatafeedSubscriptionState from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText DatafeedSubscriptionState where
  toText = \case
    DSSActive -> "Active"
    DSSInactive -> "Inactive"

instance Hashable DatafeedSubscriptionState

instance NFData DatafeedSubscriptionState

instance ToByteString DatafeedSubscriptionState

instance ToQuery DatafeedSubscriptionState

instance ToHeader DatafeedSubscriptionState

instance FromXML DatafeedSubscriptionState where
  parseXML = parseXMLText "DatafeedSubscriptionState"
