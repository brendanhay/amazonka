{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.RepoUpgradeOnBoot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.RepoUpgradeOnBoot where

import Network.AWS.Prelude

data RepoUpgradeOnBoot
  = RUOBNone
  | RUOBSecurity
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

instance FromText RepoUpgradeOnBoot where
  parser =
    takeLowerText >>= \case
      "none" -> pure RUOBNone
      "security" -> pure RUOBSecurity
      e ->
        fromTextError $
          "Failure parsing RepoUpgradeOnBoot from value: '" <> e
            <> "'. Accepted values: none, security"

instance ToText RepoUpgradeOnBoot where
  toText = \case
    RUOBNone -> "NONE"
    RUOBSecurity -> "SECURITY"

instance Hashable RepoUpgradeOnBoot

instance NFData RepoUpgradeOnBoot

instance ToByteString RepoUpgradeOnBoot

instance ToQuery RepoUpgradeOnBoot

instance ToHeader RepoUpgradeOnBoot

instance ToJSON RepoUpgradeOnBoot where
  toJSON = toJSONText

instance FromJSON RepoUpgradeOnBoot where
  parseJSON = parseJSONText "RepoUpgradeOnBoot"
