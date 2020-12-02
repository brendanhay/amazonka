{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Engine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Engine where

import Network.AWS.Prelude

data Engine
  = Neural
  | Standard
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

instance FromText Engine where
  parser =
    takeLowerText >>= \case
      "neural" -> pure Neural
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing Engine from value: '" <> e
            <> "'. Accepted values: neural, standard"

instance ToText Engine where
  toText = \case
    Neural -> "neural"
    Standard -> "standard"

instance Hashable Engine

instance NFData Engine

instance ToByteString Engine

instance ToQuery Engine

instance ToHeader Engine

instance ToJSON Engine where
  toJSON = toJSONText

instance FromJSON Engine where
  parseJSON = parseJSONText "Engine"
