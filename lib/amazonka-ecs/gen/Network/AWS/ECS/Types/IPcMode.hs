{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.IPcMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.IPcMode where

import Network.AWS.Prelude

data IPcMode
  = IMHost
  | IMNone
  | IMTask
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

instance FromText IPcMode where
  parser =
    takeLowerText >>= \case
      "host" -> pure IMHost
      "none" -> pure IMNone
      "task" -> pure IMTask
      e ->
        fromTextError $
          "Failure parsing IPcMode from value: '" <> e
            <> "'. Accepted values: host, none, task"

instance ToText IPcMode where
  toText = \case
    IMHost -> "host"
    IMNone -> "none"
    IMTask -> "task"

instance Hashable IPcMode

instance NFData IPcMode

instance ToByteString IPcMode

instance ToQuery IPcMode

instance ToHeader IPcMode

instance ToJSON IPcMode where
  toJSON = toJSONText

instance FromJSON IPcMode where
  parseJSON = parseJSONText "IPcMode"
