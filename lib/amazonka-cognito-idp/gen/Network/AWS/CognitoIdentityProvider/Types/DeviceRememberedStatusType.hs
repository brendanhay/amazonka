{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType where

import Network.AWS.Prelude

data DeviceRememberedStatusType
  = NotRemembered
  | Remembered
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

instance FromText DeviceRememberedStatusType where
  parser =
    takeLowerText >>= \case
      "not_remembered" -> pure NotRemembered
      "remembered" -> pure Remembered
      e ->
        fromTextError $
          "Failure parsing DeviceRememberedStatusType from value: '" <> e
            <> "'. Accepted values: not_remembered, remembered"

instance ToText DeviceRememberedStatusType where
  toText = \case
    NotRemembered -> "not_remembered"
    Remembered -> "remembered"

instance Hashable DeviceRememberedStatusType

instance NFData DeviceRememberedStatusType

instance ToByteString DeviceRememberedStatusType

instance ToQuery DeviceRememberedStatusType

instance ToHeader DeviceRememberedStatusType

instance ToJSON DeviceRememberedStatusType where
  toJSON = toJSONText
