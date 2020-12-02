{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.DataSource where

import Network.AWS.Prelude

data DataSource = Agent
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

instance FromText DataSource where
  parser =
    takeLowerText >>= \case
      "agent" -> pure Agent
      e ->
        fromTextError $
          "Failure parsing DataSource from value: '" <> e
            <> "'. Accepted values: agent"

instance ToText DataSource where
  toText = \case
    Agent -> "AGENT"

instance Hashable DataSource

instance NFData DataSource

instance ToByteString DataSource

instance ToQuery DataSource

instance ToHeader DataSource

instance FromJSON DataSource where
  parseJSON = parseJSONText "DataSource"
