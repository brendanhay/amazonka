{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ExecutionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ExecutionMode where

import Network.AWS.Prelude

data ExecutionMode
  = EMAuto
  | EMInteractive
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

instance FromText ExecutionMode where
  parser =
    takeLowerText >>= \case
      "auto" -> pure EMAuto
      "interactive" -> pure EMInteractive
      e ->
        fromTextError $
          "Failure parsing ExecutionMode from value: '" <> e
            <> "'. Accepted values: auto, interactive"

instance ToText ExecutionMode where
  toText = \case
    EMAuto -> "Auto"
    EMInteractive -> "Interactive"

instance Hashable ExecutionMode

instance NFData ExecutionMode

instance ToByteString ExecutionMode

instance ToQuery ExecutionMode

instance ToHeader ExecutionMode

instance ToJSON ExecutionMode where
  toJSON = toJSONText

instance FromJSON ExecutionMode where
  parseJSON = parseJSONText "ExecutionMode"
