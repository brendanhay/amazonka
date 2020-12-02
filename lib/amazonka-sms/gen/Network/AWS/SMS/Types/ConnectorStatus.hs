{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ConnectorStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ConnectorStatus where

import Network.AWS.Prelude

data ConnectorStatus
  = Healthy
  | Unhealthy
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

instance FromText ConnectorStatus where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure Healthy
      "unhealthy" -> pure Unhealthy
      e ->
        fromTextError $
          "Failure parsing ConnectorStatus from value: '" <> e
            <> "'. Accepted values: healthy, unhealthy"

instance ToText ConnectorStatus where
  toText = \case
    Healthy -> "HEALTHY"
    Unhealthy -> "UNHEALTHY"

instance Hashable ConnectorStatus

instance NFData ConnectorStatus

instance ToByteString ConnectorStatus

instance ToQuery ConnectorStatus

instance ToHeader ConnectorStatus

instance FromJSON ConnectorStatus where
  parseJSON = parseJSONText "ConnectorStatus"
