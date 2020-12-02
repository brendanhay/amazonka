{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RelationalDatabaseSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RelationalDatabaseSourceType where

import Network.AWS.Prelude

data RelationalDatabaseSourceType = RDSHTTPEndpoint
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

instance FromText RelationalDatabaseSourceType where
  parser =
    takeLowerText >>= \case
      "rds_http_endpoint" -> pure RDSHTTPEndpoint
      e ->
        fromTextError $
          "Failure parsing RelationalDatabaseSourceType from value: '" <> e
            <> "'. Accepted values: rds_http_endpoint"

instance ToText RelationalDatabaseSourceType where
  toText = \case
    RDSHTTPEndpoint -> "RDS_HTTP_ENDPOINT"

instance Hashable RelationalDatabaseSourceType

instance NFData RelationalDatabaseSourceType

instance ToByteString RelationalDatabaseSourceType

instance ToQuery RelationalDatabaseSourceType

instance ToHeader RelationalDatabaseSourceType

instance ToJSON RelationalDatabaseSourceType where
  toJSON = toJSONText

instance FromJSON RelationalDatabaseSourceType where
  parseJSON = parseJSONText "RelationalDatabaseSourceType"
