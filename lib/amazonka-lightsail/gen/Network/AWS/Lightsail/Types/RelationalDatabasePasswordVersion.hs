{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion where

import Network.AWS.Prelude

data RelationalDatabasePasswordVersion
  = RDPVCurrent
  | RDPVPending
  | RDPVPrevious
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

instance FromText RelationalDatabasePasswordVersion where
  parser =
    takeLowerText >>= \case
      "current" -> pure RDPVCurrent
      "pending" -> pure RDPVPending
      "previous" -> pure RDPVPrevious
      e ->
        fromTextError $
          "Failure parsing RelationalDatabasePasswordVersion from value: '" <> e
            <> "'. Accepted values: current, pending, previous"

instance ToText RelationalDatabasePasswordVersion where
  toText = \case
    RDPVCurrent -> "CURRENT"
    RDPVPending -> "PENDING"
    RDPVPrevious -> "PREVIOUS"

instance Hashable RelationalDatabasePasswordVersion

instance NFData RelationalDatabasePasswordVersion

instance ToByteString RelationalDatabasePasswordVersion

instance ToQuery RelationalDatabasePasswordVersion

instance ToHeader RelationalDatabasePasswordVersion

instance ToJSON RelationalDatabasePasswordVersion where
  toJSON = toJSONText
