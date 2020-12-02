{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneType where

import Network.AWS.Prelude

data PhoneType
  = DeskPhone
  | SoftPhone
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

instance FromText PhoneType where
  parser =
    takeLowerText >>= \case
      "desk_phone" -> pure DeskPhone
      "soft_phone" -> pure SoftPhone
      e ->
        fromTextError $
          "Failure parsing PhoneType from value: '" <> e
            <> "'. Accepted values: desk_phone, soft_phone"

instance ToText PhoneType where
  toText = \case
    DeskPhone -> "DESK_PHONE"
    SoftPhone -> "SOFT_PHONE"

instance Hashable PhoneType

instance NFData PhoneType

instance ToByteString PhoneType

instance ToQuery PhoneType

instance ToHeader PhoneType

instance ToJSON PhoneType where
  toJSON = toJSONText

instance FromJSON PhoneType where
  parseJSON = parseJSONText "PhoneType"
