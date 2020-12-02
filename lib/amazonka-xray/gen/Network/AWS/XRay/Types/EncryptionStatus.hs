{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EncryptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EncryptionStatus where

import Network.AWS.Prelude

data EncryptionStatus
  = Active
  | Updating
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

instance FromText EncryptionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing EncryptionStatus from value: '" <> e
            <> "'. Accepted values: active, updating"

instance ToText EncryptionStatus where
  toText = \case
    Active -> "ACTIVE"
    Updating -> "UPDATING"

instance Hashable EncryptionStatus

instance NFData EncryptionStatus

instance ToByteString EncryptionStatus

instance ToQuery EncryptionStatus

instance ToHeader EncryptionStatus

instance FromJSON EncryptionStatus where
  parseJSON = parseJSONText "EncryptionStatus"
