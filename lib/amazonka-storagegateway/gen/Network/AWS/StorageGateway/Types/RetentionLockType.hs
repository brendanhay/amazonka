{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.RetentionLockType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.RetentionLockType where

import Network.AWS.Prelude

data RetentionLockType
  = Compliance
  | Governance
  | None
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

instance FromText RetentionLockType where
  parser =
    takeLowerText >>= \case
      "compliance" -> pure Compliance
      "governance" -> pure Governance
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing RetentionLockType from value: '" <> e
            <> "'. Accepted values: compliance, governance, none"

instance ToText RetentionLockType where
  toText = \case
    Compliance -> "COMPLIANCE"
    Governance -> "GOVERNANCE"
    None -> "NONE"

instance Hashable RetentionLockType

instance NFData RetentionLockType

instance ToByteString RetentionLockType

instance ToQuery RetentionLockType

instance ToHeader RetentionLockType

instance ToJSON RetentionLockType where
  toJSON = toJSONText

instance FromJSON RetentionLockType where
  parseJSON = parseJSONText "RetentionLockType"
