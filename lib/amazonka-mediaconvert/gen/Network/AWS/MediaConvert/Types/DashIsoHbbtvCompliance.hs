{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance where

import Network.AWS.Prelude

-- | Supports HbbTV specification as indicated
data DashIsoHbbtvCompliance
  = DIHCHbbtv15
  | DIHCNone
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

instance FromText DashIsoHbbtvCompliance where
  parser =
    takeLowerText >>= \case
      "hbbtv_1_5" -> pure DIHCHbbtv15
      "none" -> pure DIHCNone
      e ->
        fromTextError $
          "Failure parsing DashIsoHbbtvCompliance from value: '" <> e
            <> "'. Accepted values: hbbtv_1_5, none"

instance ToText DashIsoHbbtvCompliance where
  toText = \case
    DIHCHbbtv15 -> "HBBTV_1_5"
    DIHCNone -> "NONE"

instance Hashable DashIsoHbbtvCompliance

instance NFData DashIsoHbbtvCompliance

instance ToByteString DashIsoHbbtvCompliance

instance ToQuery DashIsoHbbtvCompliance

instance ToHeader DashIsoHbbtvCompliance

instance ToJSON DashIsoHbbtvCompliance where
  toJSON = toJSONText

instance FromJSON DashIsoHbbtvCompliance where
  parseJSON = parseJSONText "DashIsoHbbtvCompliance"
