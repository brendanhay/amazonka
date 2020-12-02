{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerComponent where

import Network.AWS.Prelude

data LoggerComponent
  = GreengrassSystem
  | Lambda
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

instance FromText LoggerComponent where
  parser =
    takeLowerText >>= \case
      "greengrasssystem" -> pure GreengrassSystem
      "lambda" -> pure Lambda
      e ->
        fromTextError $
          "Failure parsing LoggerComponent from value: '" <> e
            <> "'. Accepted values: greengrasssystem, lambda"

instance ToText LoggerComponent where
  toText = \case
    GreengrassSystem -> "GreengrassSystem"
    Lambda -> "Lambda"

instance Hashable LoggerComponent

instance NFData LoggerComponent

instance ToByteString LoggerComponent

instance ToQuery LoggerComponent

instance ToHeader LoggerComponent

instance ToJSON LoggerComponent where
  toJSON = toJSONText

instance FromJSON LoggerComponent where
  parseJSON = parseJSONText "LoggerComponent"
