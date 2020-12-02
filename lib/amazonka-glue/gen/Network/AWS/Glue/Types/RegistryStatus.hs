{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RegistryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RegistryStatus where

import Network.AWS.Prelude

data RegistryStatus
  = RSAvailable
  | RSDeleting
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

instance FromText RegistryStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure RSAvailable
      "deleting" -> pure RSDeleting
      e ->
        fromTextError $
          "Failure parsing RegistryStatus from value: '" <> e
            <> "'. Accepted values: available, deleting"

instance ToText RegistryStatus where
  toText = \case
    RSAvailable -> "AVAILABLE"
    RSDeleting -> "DELETING"

instance Hashable RegistryStatus

instance NFData RegistryStatus

instance ToByteString RegistryStatus

instance ToQuery RegistryStatus

instance ToHeader RegistryStatus

instance FromJSON RegistryStatus where
  parseJSON = parseJSONText "RegistryStatus"
