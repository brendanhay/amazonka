{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.FilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.FilterType where

import Network.AWS.Prelude

data FilterType
  = Endpoint
  | System
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

instance FromText FilterType where
  parser =
    takeLowerText >>= \case
      "endpoint" -> pure Endpoint
      "system" -> pure System
      e ->
        fromTextError $
          "Failure parsing FilterType from value: '" <> e
            <> "'. Accepted values: endpoint, system"

instance ToText FilterType where
  toText = \case
    Endpoint -> "ENDPOINT"
    System -> "SYSTEM"

instance Hashable FilterType

instance NFData FilterType

instance ToByteString FilterType

instance ToQuery FilterType

instance ToHeader FilterType

instance ToJSON FilterType where
  toJSON = toJSONText

instance FromJSON FilterType where
  parseJSON = parseJSONText "FilterType"
