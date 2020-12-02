{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AutoRegistrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AutoRegistrationStatus where

import Network.AWS.Prelude

data AutoRegistrationStatus
  = Disable
  | Enable
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

instance FromText AutoRegistrationStatus where
  parser =
    takeLowerText >>= \case
      "disable" -> pure Disable
      "enable" -> pure Enable
      e ->
        fromTextError $
          "Failure parsing AutoRegistrationStatus from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText AutoRegistrationStatus where
  toText = \case
    Disable -> "DISABLE"
    Enable -> "ENABLE"

instance Hashable AutoRegistrationStatus

instance NFData AutoRegistrationStatus

instance ToByteString AutoRegistrationStatus

instance ToQuery AutoRegistrationStatus

instance ToHeader AutoRegistrationStatus

instance ToJSON AutoRegistrationStatus where
  toJSON = toJSONText

instance FromJSON AutoRegistrationStatus where
  parseJSON = parseJSONText "AutoRegistrationStatus"
