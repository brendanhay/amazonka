{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkerType where

import Network.AWS.Prelude

data WorkerType
  = G_1X
  | G_2X
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

instance FromText WorkerType where
  parser =
    takeLowerText >>= \case
      "g.1x" -> pure G_1X
      "g.2x" -> pure G_2X
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing WorkerType from value: '" <> e
            <> "'. Accepted values: g.1x, g.2x, standard"

instance ToText WorkerType where
  toText = \case
    G_1X -> "G.1X"
    G_2X -> "G.2X"
    Standard -> "Standard"

instance Hashable WorkerType

instance NFData WorkerType

instance ToByteString WorkerType

instance ToQuery WorkerType

instance ToHeader WorkerType

instance ToJSON WorkerType where
  toJSON = toJSONText

instance FromJSON WorkerType where
  parseJSON = parseJSONText "WorkerType"
