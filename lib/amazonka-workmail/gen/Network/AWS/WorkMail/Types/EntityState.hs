{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.EntityState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.EntityState where

import Network.AWS.Prelude

data EntityState
  = Deleted
  | Disabled
  | Enabled
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

instance FromText EntityState where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure Deleted
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing EntityState from value: '" <> e
            <> "'. Accepted values: deleted, disabled, enabled"

instance ToText EntityState where
  toText = \case
    Deleted -> "DELETED"
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable EntityState

instance NFData EntityState

instance ToByteString EntityState

instance ToQuery EntityState

instance ToHeader EntityState

instance FromJSON EntityState where
  parseJSON = parseJSONText "EntityState"
