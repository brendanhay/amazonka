{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceHealthStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceHealthStatus
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

instance FromText InstanceHealthStatus where
  parser =
    takeLowerText >>= \case
      "healthy" -> pure Healthy
      "unhealthy" -> pure Unhealthy
      e ->
        fromTextError $
          "Failure parsing InstanceHealthStatus from value: '" <> e
            <> "'. Accepted values: healthy, unhealthy"

instance ToText InstanceHealthStatus where
  toText = \case
    Healthy -> "healthy"
    Unhealthy -> "unhealthy"

instance Hashable InstanceHealthStatus

instance NFData InstanceHealthStatus

instance ToByteString InstanceHealthStatus

instance ToQuery InstanceHealthStatus

instance ToHeader InstanceHealthStatus

instance FromXML InstanceHealthStatus where
  parseXML = parseXMLText "InstanceHealthStatus"
