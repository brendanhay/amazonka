{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAliasState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAliasState where

import Network.AWS.Prelude

data ConnectionAliasState
  = Created
  | Creating
  | Deleting
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

instance FromText ConnectionAliasState where
  parser =
    takeLowerText >>= \case
      "created" -> pure Created
      "creating" -> pure Creating
      "deleting" -> pure Deleting
      e ->
        fromTextError $
          "Failure parsing ConnectionAliasState from value: '" <> e
            <> "'. Accepted values: created, creating, deleting"

instance ToText ConnectionAliasState where
  toText = \case
    Created -> "CREATED"
    Creating -> "CREATING"
    Deleting -> "DELETING"

instance Hashable ConnectionAliasState

instance NFData ConnectionAliasState

instance ToByteString ConnectionAliasState

instance ToQuery ConnectionAliasState

instance ToHeader ConnectionAliasState

instance FromJSON ConnectionAliasState where
  parseJSON = parseJSONText "ConnectionAliasState"
