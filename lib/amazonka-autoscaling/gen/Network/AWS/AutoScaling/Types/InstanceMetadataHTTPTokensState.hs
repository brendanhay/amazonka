{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMetadataHTTPTokensState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMetadataHTTPTokensState where

import Network.AWS.Prelude

data InstanceMetadataHTTPTokensState
  = Optional
  | Required
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

instance FromText InstanceMetadataHTTPTokensState where
  parser =
    takeLowerText >>= \case
      "optional" -> pure Optional
      "required" -> pure Required
      e ->
        fromTextError $
          "Failure parsing InstanceMetadataHTTPTokensState from value: '" <> e
            <> "'. Accepted values: optional, required"

instance ToText InstanceMetadataHTTPTokensState where
  toText = \case
    Optional -> "optional"
    Required -> "required"

instance Hashable InstanceMetadataHTTPTokensState

instance NFData InstanceMetadataHTTPTokensState

instance ToByteString InstanceMetadataHTTPTokensState

instance ToQuery InstanceMetadataHTTPTokensState

instance ToHeader InstanceMetadataHTTPTokensState

instance FromXML InstanceMetadataHTTPTokensState where
  parseXML = parseXMLText "InstanceMetadataHTTPTokensState"
