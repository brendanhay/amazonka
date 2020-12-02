{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionIsolationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionIsolationMode where

import Network.AWS.Prelude

-- | Specifies whether the Lambda function runs in a Greengrass container (default) or without containerization. Unless your scenario requires that you run without containerization, we recommend that you run in a Greengrass container. Omit this value to run the Lambda function with the default containerization for the group.
data FunctionIsolationMode
  = GreengrassContainer
  | NoContainer
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

instance FromText FunctionIsolationMode where
  parser =
    takeLowerText >>= \case
      "greengrasscontainer" -> pure GreengrassContainer
      "nocontainer" -> pure NoContainer
      e ->
        fromTextError $
          "Failure parsing FunctionIsolationMode from value: '" <> e
            <> "'. Accepted values: greengrasscontainer, nocontainer"

instance ToText FunctionIsolationMode where
  toText = \case
    GreengrassContainer -> "GreengrassContainer"
    NoContainer -> "NoContainer"

instance Hashable FunctionIsolationMode

instance NFData FunctionIsolationMode

instance ToByteString FunctionIsolationMode

instance ToQuery FunctionIsolationMode

instance ToHeader FunctionIsolationMode

instance ToJSON FunctionIsolationMode where
  toJSON = toJSONText

instance FromJSON FunctionIsolationMode where
  parseJSON = parseJSONText "FunctionIsolationMode"
