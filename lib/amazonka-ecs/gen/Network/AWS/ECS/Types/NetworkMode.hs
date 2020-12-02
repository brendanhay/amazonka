{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkMode where

import Network.AWS.Prelude

data NetworkMode
  = AWSvpc
  | Bridge
  | Host
  | None
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

instance FromText NetworkMode where
  parser =
    takeLowerText >>= \case
      "awsvpc" -> pure AWSvpc
      "bridge" -> pure Bridge
      "host" -> pure Host
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing NetworkMode from value: '" <> e
            <> "'. Accepted values: awsvpc, bridge, host, none"

instance ToText NetworkMode where
  toText = \case
    AWSvpc -> "awsvpc"
    Bridge -> "bridge"
    Host -> "host"
    None -> "none"

instance Hashable NetworkMode

instance NFData NetworkMode

instance ToByteString NetworkMode

instance ToQuery NetworkMode

instance ToHeader NetworkMode

instance ToJSON NetworkMode where
  toJSON = toJSONText

instance FromJSON NetworkMode where
  parseJSON = parseJSONText "NetworkMode"
