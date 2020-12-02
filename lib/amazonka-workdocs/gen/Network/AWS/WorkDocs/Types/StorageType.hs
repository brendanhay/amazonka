{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.StorageType where

import Network.AWS.Prelude

data StorageType
  = Quota
  | Unlimited
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

instance FromText StorageType where
  parser =
    takeLowerText >>= \case
      "quota" -> pure Quota
      "unlimited" -> pure Unlimited
      e ->
        fromTextError $
          "Failure parsing StorageType from value: '" <> e
            <> "'. Accepted values: quota, unlimited"

instance ToText StorageType where
  toText = \case
    Quota -> "QUOTA"
    Unlimited -> "UNLIMITED"

instance Hashable StorageType

instance NFData StorageType

instance ToByteString StorageType

instance ToQuery StorageType

instance ToHeader StorageType

instance ToJSON StorageType where
  toJSON = toJSONText

instance FromJSON StorageType where
  parseJSON = parseJSONText "StorageType"
