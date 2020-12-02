{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventResponseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventResponseType where

import Network.AWS.Prelude

data EventResponseType
  = Failure
  | Success
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

instance FromText EventResponseType where
  parser =
    takeLowerText >>= \case
      "failure" -> pure Failure
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing EventResponseType from value: '" <> e
            <> "'. Accepted values: failure, success"

instance ToText EventResponseType where
  toText = \case
    Failure -> "Failure"
    Success -> "Success"

instance Hashable EventResponseType

instance NFData EventResponseType

instance ToByteString EventResponseType

instance ToQuery EventResponseType

instance ToHeader EventResponseType

instance FromJSON EventResponseType where
  parseJSON = parseJSONText "EventResponseType"
