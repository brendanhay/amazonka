{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ReplicationScope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ReplicationScope where

import Network.AWS.Prelude

data ReplicationScope = Domain
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

instance FromText ReplicationScope where
  parser =
    takeLowerText >>= \case
      "domain" -> pure Domain
      e ->
        fromTextError $
          "Failure parsing ReplicationScope from value: '" <> e
            <> "'. Accepted values: domain"

instance ToText ReplicationScope where
  toText = \case
    Domain -> "Domain"

instance Hashable ReplicationScope

instance NFData ReplicationScope

instance ToByteString ReplicationScope

instance ToQuery ReplicationScope

instance ToHeader ReplicationScope

instance FromJSON ReplicationScope where
  parseJSON = parseJSONText "ReplicationScope"
