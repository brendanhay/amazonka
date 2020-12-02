{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.AccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.AccountStatus where

import Network.AWS.Prelude

data AccountStatus
  = Active
  | Suspended
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

instance FromText AccountStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "suspended" -> pure Suspended
      e ->
        fromTextError $
          "Failure parsing AccountStatus from value: '" <> e
            <> "'. Accepted values: active, suspended"

instance ToText AccountStatus where
  toText = \case
    Active -> "ACTIVE"
    Suspended -> "SUSPENDED"

instance Hashable AccountStatus

instance NFData AccountStatus

instance ToByteString AccountStatus

instance ToQuery AccountStatus

instance ToHeader AccountStatus

instance FromJSON AccountStatus where
  parseJSON = parseJSONText "AccountStatus"
