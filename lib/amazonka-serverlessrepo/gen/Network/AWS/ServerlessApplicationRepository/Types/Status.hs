{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Status
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Status where

import Network.AWS.Prelude

data Status
  = Active
  | Expired
  | Preparing
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

instance FromText Status where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "expired" -> pure Expired
      "preparing" -> pure Preparing
      e ->
        fromTextError $
          "Failure parsing Status from value: '" <> e
            <> "'. Accepted values: active, expired, preparing"

instance ToText Status where
  toText = \case
    Active -> "ACTIVE"
    Expired -> "EXPIRED"
    Preparing -> "PREPARING"

instance Hashable Status

instance NFData Status

instance ToByteString Status

instance ToQuery Status

instance ToHeader Status

instance FromJSON Status where
  parseJSON = parseJSONText "Status"
