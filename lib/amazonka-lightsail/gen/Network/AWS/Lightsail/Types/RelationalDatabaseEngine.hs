{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEngine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEngine where

import Network.AWS.Prelude

data RelationalDatabaseEngine = Mysql
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

instance FromText RelationalDatabaseEngine where
  parser =
    takeLowerText >>= \case
      "mysql" -> pure Mysql
      e ->
        fromTextError $
          "Failure parsing RelationalDatabaseEngine from value: '" <> e
            <> "'. Accepted values: mysql"

instance ToText RelationalDatabaseEngine where
  toText = \case
    Mysql -> "mysql"

instance Hashable RelationalDatabaseEngine

instance NFData RelationalDatabaseEngine

instance ToByteString RelationalDatabaseEngine

instance ToQuery RelationalDatabaseEngine

instance ToHeader RelationalDatabaseEngine

instance FromJSON RelationalDatabaseEngine where
  parseJSON = parseJSONText "RelationalDatabaseEngine"
