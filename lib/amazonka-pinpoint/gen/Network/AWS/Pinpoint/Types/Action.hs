{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Action where

import Network.AWS.Prelude

data Action
  = DeepLink
  | OpenApp
  | URL
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

instance FromText Action where
  parser =
    takeLowerText >>= \case
      "deep_link" -> pure DeepLink
      "open_app" -> pure OpenApp
      "url" -> pure URL
      e ->
        fromTextError $
          "Failure parsing Action from value: '" <> e
            <> "'. Accepted values: deep_link, open_app, url"

instance ToText Action where
  toText = \case
    DeepLink -> "DEEP_LINK"
    OpenApp -> "OPEN_APP"
    URL -> "URL"

instance Hashable Action

instance NFData Action

instance ToByteString Action

instance ToQuery Action

instance ToHeader Action

instance ToJSON Action where
  toJSON = toJSONText

instance FromJSON Action where
  parseJSON = parseJSONText "Action"
