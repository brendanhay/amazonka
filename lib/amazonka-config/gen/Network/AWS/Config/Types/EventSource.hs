{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.EventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.EventSource where

import Network.AWS.Prelude

data EventSource = AWS_Config
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

instance FromText EventSource where
  parser =
    takeLowerText >>= \case
      "aws.config" -> pure AWS_Config
      e ->
        fromTextError $
          "Failure parsing EventSource from value: '" <> e
            <> "'. Accepted values: aws.config"

instance ToText EventSource where
  toText = \case
    AWS_Config -> "aws.config"

instance Hashable EventSource

instance NFData EventSource

instance ToByteString EventSource

instance ToQuery EventSource

instance ToHeader EventSource

instance ToJSON EventSource where
  toJSON = toJSONText

instance FromJSON EventSource where
  parseJSON = parseJSONText "EventSource"
