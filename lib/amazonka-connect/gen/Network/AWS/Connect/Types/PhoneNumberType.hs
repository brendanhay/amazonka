{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberType where

import Network.AWS.Prelude

data PhoneNumberType
  = Did
  | TollFree
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

instance FromText PhoneNumberType where
  parser =
    takeLowerText >>= \case
      "did" -> pure Did
      "toll_free" -> pure TollFree
      e ->
        fromTextError $
          "Failure parsing PhoneNumberType from value: '" <> e
            <> "'. Accepted values: did, toll_free"

instance ToText PhoneNumberType where
  toText = \case
    Did -> "DID"
    TollFree -> "TOLL_FREE"

instance Hashable PhoneNumberType

instance NFData PhoneNumberType

instance ToByteString PhoneNumberType

instance ToQuery PhoneNumberType

instance ToHeader PhoneNumberType

instance ToJSON PhoneNumberType where
  toJSON = toJSONText

instance FromJSON PhoneNumberType where
  parseJSON = parseJSONText "PhoneNumberType"
