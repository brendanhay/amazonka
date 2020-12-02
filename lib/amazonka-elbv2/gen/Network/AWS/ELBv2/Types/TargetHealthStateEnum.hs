{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthStateEnum where

import Network.AWS.Prelude

data TargetHealthStateEnum
  = Draining
  | Healthy
  | Initial
  | Unavailable
  | Unhealthy
  | Unused
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

instance FromText TargetHealthStateEnum where
  parser =
    takeLowerText >>= \case
      "draining" -> pure Draining
      "healthy" -> pure Healthy
      "initial" -> pure Initial
      "unavailable" -> pure Unavailable
      "unhealthy" -> pure Unhealthy
      "unused" -> pure Unused
      e ->
        fromTextError $
          "Failure parsing TargetHealthStateEnum from value: '" <> e
            <> "'. Accepted values: draining, healthy, initial, unavailable, unhealthy, unused"

instance ToText TargetHealthStateEnum where
  toText = \case
    Draining -> "draining"
    Healthy -> "healthy"
    Initial -> "initial"
    Unavailable -> "unavailable"
    Unhealthy -> "unhealthy"
    Unused -> "unused"

instance Hashable TargetHealthStateEnum

instance NFData TargetHealthStateEnum

instance ToByteString TargetHealthStateEnum

instance ToQuery TargetHealthStateEnum

instance ToHeader TargetHealthStateEnum

instance FromXML TargetHealthStateEnum where
  parseXML = parseXMLText "TargetHealthStateEnum"
