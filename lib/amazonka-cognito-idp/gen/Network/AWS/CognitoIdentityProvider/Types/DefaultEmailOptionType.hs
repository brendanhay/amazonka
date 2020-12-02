{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType where

import Network.AWS.Prelude

data DefaultEmailOptionType
  = ConfirmWithCode
  | ConfirmWithLink
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

instance FromText DefaultEmailOptionType where
  parser =
    takeLowerText >>= \case
      "confirm_with_code" -> pure ConfirmWithCode
      "confirm_with_link" -> pure ConfirmWithLink
      e ->
        fromTextError $
          "Failure parsing DefaultEmailOptionType from value: '" <> e
            <> "'. Accepted values: confirm_with_code, confirm_with_link"

instance ToText DefaultEmailOptionType where
  toText = \case
    ConfirmWithCode -> "CONFIRM_WITH_CODE"
    ConfirmWithLink -> "CONFIRM_WITH_LINK"

instance Hashable DefaultEmailOptionType

instance NFData DefaultEmailOptionType

instance ToByteString DefaultEmailOptionType

instance ToQuery DefaultEmailOptionType

instance ToHeader DefaultEmailOptionType

instance ToJSON DefaultEmailOptionType where
  toJSON = toJSONText

instance FromJSON DefaultEmailOptionType where
  parseJSON = parseJSONText "DefaultEmailOptionType"
