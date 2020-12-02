{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumberType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumberType where

import Network.AWS.Prelude

data PhoneNumberType
  = PNTHome
  | PNTMobile
  | PNTWork
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable, Generic)

instance FromText PhoneNumberType where
  parser =
    takeLowerText >>= \case
      "home" -> pure PNTHome
      "mobile" -> pure PNTMobile
      "work" -> pure PNTWork
      e ->
        fromTextError $
          "Failure parsing PhoneNumberType from value: '" <> e
            <> "'. Accepted values: home, mobile, work"

instance ToText PhoneNumberType where
  toText = \case
    PNTHome -> "HOME"
    PNTMobile -> "MOBILE"
    PNTWork -> "WORK"

instance Hashable PhoneNumberType

instance NFData PhoneNumberType

instance ToByteString PhoneNumberType

instance ToQuery PhoneNumberType

instance ToHeader PhoneNumberType

instance ToJSON PhoneNumberType where
  toJSON = toJSONText

instance FromJSON PhoneNumberType where
  parseJSON = parseJSONText "PhoneNumberType"
