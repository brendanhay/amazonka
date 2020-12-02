{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StreamView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StreamView where

import Network.AWS.Prelude

data StreamView
  = App
  | Desktop
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

instance FromText StreamView where
  parser =
    takeLowerText >>= \case
      "app" -> pure App
      "desktop" -> pure Desktop
      e ->
        fromTextError $
          "Failure parsing StreamView from value: '" <> e
            <> "'. Accepted values: app, desktop"

instance ToText StreamView where
  toText = \case
    App -> "APP"
    Desktop -> "DESKTOP"

instance Hashable StreamView

instance NFData StreamView

instance ToByteString StreamView

instance ToQuery StreamView

instance ToHeader StreamView

instance ToJSON StreamView where
  toJSON = toJSONText

instance FromJSON StreamView where
  parseJSON = parseJSONText "StreamView"
