{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SafeguardPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SafeguardPolicy where

import Network.AWS.Prelude

data SafeguardPolicy
  = ExclusiveAutomaticTruncation
  | RelyOnSqlServerReplicationAgent
  | SharedAutomaticTruncation
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

instance FromText SafeguardPolicy where
  parser =
    takeLowerText >>= \case
      "exclusive-automatic-truncation" -> pure ExclusiveAutomaticTruncation
      "rely-on-sql-server-replication-agent" -> pure RelyOnSqlServerReplicationAgent
      "shared-automatic-truncation" -> pure SharedAutomaticTruncation
      e ->
        fromTextError $
          "Failure parsing SafeguardPolicy from value: '" <> e
            <> "'. Accepted values: exclusive-automatic-truncation, rely-on-sql-server-replication-agent, shared-automatic-truncation"

instance ToText SafeguardPolicy where
  toText = \case
    ExclusiveAutomaticTruncation -> "exclusive-automatic-truncation"
    RelyOnSqlServerReplicationAgent -> "rely-on-sql-server-replication-agent"
    SharedAutomaticTruncation -> "shared-automatic-truncation"

instance Hashable SafeguardPolicy

instance NFData SafeguardPolicy

instance ToByteString SafeguardPolicy

instance ToQuery SafeguardPolicy

instance ToHeader SafeguardPolicy

instance ToJSON SafeguardPolicy where
  toJSON = toJSONText

instance FromJSON SafeguardPolicy where
  parseJSON = parseJSONText "SafeguardPolicy"
