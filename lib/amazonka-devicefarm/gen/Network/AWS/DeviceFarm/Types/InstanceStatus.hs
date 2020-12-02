{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceStatus where

import Network.AWS.Prelude

data InstanceStatus
  = ISAvailable
  | ISInUse
  | ISNotAvailable
  | ISPreparing
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

instance FromText InstanceStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure ISAvailable
      "in_use" -> pure ISInUse
      "not_available" -> pure ISNotAvailable
      "preparing" -> pure ISPreparing
      e ->
        fromTextError $
          "Failure parsing InstanceStatus from value: '" <> e
            <> "'. Accepted values: available, in_use, not_available, preparing"

instance ToText InstanceStatus where
  toText = \case
    ISAvailable -> "AVAILABLE"
    ISInUse -> "IN_USE"
    ISNotAvailable -> "NOT_AVAILABLE"
    ISPreparing -> "PREPARING"

instance Hashable InstanceStatus

instance NFData InstanceStatus

instance ToByteString InstanceStatus

instance ToQuery InstanceStatus

instance ToHeader InstanceStatus

instance FromJSON InstanceStatus where
  parseJSON = parseJSONText "InstanceStatus"
