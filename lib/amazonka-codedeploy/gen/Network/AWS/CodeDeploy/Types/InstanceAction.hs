{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceAction where

import Network.AWS.Prelude

data InstanceAction
  = KeepAlive
  | Terminate
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

instance FromText InstanceAction where
  parser =
    takeLowerText >>= \case
      "keep_alive" -> pure KeepAlive
      "terminate" -> pure Terminate
      e ->
        fromTextError $
          "Failure parsing InstanceAction from value: '" <> e
            <> "'. Accepted values: keep_alive, terminate"

instance ToText InstanceAction where
  toText = \case
    KeepAlive -> "KEEP_ALIVE"
    Terminate -> "TERMINATE"

instance Hashable InstanceAction

instance NFData InstanceAction

instance ToByteString InstanceAction

instance ToQuery InstanceAction

instance ToHeader InstanceAction

instance ToJSON InstanceAction where
  toJSON = toJSONText

instance FromJSON InstanceAction where
  parseJSON = parseJSONText "InstanceAction"
