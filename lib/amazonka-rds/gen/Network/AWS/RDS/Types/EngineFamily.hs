{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EngineFamily where

import Network.AWS.Prelude

data EngineFamily
  = Mysql
  | Postgresql
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

instance FromText EngineFamily where
  parser =
    takeLowerText >>= \case
      "mysql" -> pure Mysql
      "postgresql" -> pure Postgresql
      e ->
        fromTextError $
          "Failure parsing EngineFamily from value: '" <> e
            <> "'. Accepted values: mysql, postgresql"

instance ToText EngineFamily where
  toText = \case
    Mysql -> "MYSQL"
    Postgresql -> "POSTGRESQL"

instance Hashable EngineFamily

instance NFData EngineFamily

instance ToByteString EngineFamily

instance ToQuery EngineFamily

instance ToHeader EngineFamily
