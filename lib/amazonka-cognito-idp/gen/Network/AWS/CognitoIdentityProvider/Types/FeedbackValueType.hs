{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType where

import Network.AWS.Prelude

data FeedbackValueType
  = Invalid
  | Valid
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

instance FromText FeedbackValueType where
  parser =
    takeLowerText >>= \case
      "invalid" -> pure Invalid
      "valid" -> pure Valid
      e ->
        fromTextError $
          "Failure parsing FeedbackValueType from value: '" <> e
            <> "'. Accepted values: invalid, valid"

instance ToText FeedbackValueType where
  toText = \case
    Invalid -> "Invalid"
    Valid -> "Valid"

instance Hashable FeedbackValueType

instance NFData FeedbackValueType

instance ToByteString FeedbackValueType

instance ToQuery FeedbackValueType

instance ToHeader FeedbackValueType

instance ToJSON FeedbackValueType where
  toJSON = toJSONText

instance FromJSON FeedbackValueType where
  parseJSON = parseJSONText "FeedbackValueType"
