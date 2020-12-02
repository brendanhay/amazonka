{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReplicaMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReplicaMode where

import Network.AWS.Prelude

data ReplicaMode
  = Mounted
  | OpenReadOnly
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

instance FromText ReplicaMode where
  parser =
    takeLowerText >>= \case
      "mounted" -> pure Mounted
      "open-read-only" -> pure OpenReadOnly
      e ->
        fromTextError $
          "Failure parsing ReplicaMode from value: '" <> e
            <> "'. Accepted values: mounted, open-read-only"

instance ToText ReplicaMode where
  toText = \case
    Mounted -> "mounted"
    OpenReadOnly -> "open-read-only"

instance Hashable ReplicaMode

instance NFData ReplicaMode

instance ToByteString ReplicaMode

instance ToQuery ReplicaMode

instance ToHeader ReplicaMode

instance FromXML ReplicaMode where
  parseXML = parseXMLText "ReplicaMode"
