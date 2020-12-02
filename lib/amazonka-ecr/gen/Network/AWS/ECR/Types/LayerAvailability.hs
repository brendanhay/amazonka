{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LayerAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerAvailability where

import Network.AWS.Prelude

data LayerAvailability
  = Available
  | Unavailable
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

instance FromText LayerAvailability where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "unavailable" -> pure Unavailable
      e ->
        fromTextError $
          "Failure parsing LayerAvailability from value: '" <> e
            <> "'. Accepted values: available, unavailable"

instance ToText LayerAvailability where
  toText = \case
    Available -> "AVAILABLE"
    Unavailable -> "UNAVAILABLE"

instance Hashable LayerAvailability

instance NFData LayerAvailability

instance ToByteString LayerAvailability

instance ToQuery LayerAvailability

instance ToHeader LayerAvailability

instance FromJSON LayerAvailability where
  parseJSON = parseJSONText "LayerAvailability"
