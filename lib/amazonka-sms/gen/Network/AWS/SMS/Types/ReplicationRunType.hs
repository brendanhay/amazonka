{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunType where

import Network.AWS.Prelude

data ReplicationRunType
  = Automatic
  | OnDemand
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

instance FromText ReplicationRunType where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure Automatic
      "on_demand" -> pure OnDemand
      e ->
        fromTextError $
          "Failure parsing ReplicationRunType from value: '" <> e
            <> "'. Accepted values: automatic, on_demand"

instance ToText ReplicationRunType where
  toText = \case
    Automatic -> "AUTOMATIC"
    OnDemand -> "ON_DEMAND"

instance Hashable ReplicationRunType

instance NFData ReplicationRunType

instance ToByteString ReplicationRunType

instance ToQuery ReplicationRunType

instance ToHeader ReplicationRunType

instance FromJSON ReplicationRunType where
  parseJSON = parseJSONText "ReplicationRunType"
