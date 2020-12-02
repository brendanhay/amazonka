{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType where

import Network.AWS.Prelude

data UserPoolMFAType
  = UPMTON
  | UPMTOff
  | UPMTOptional
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

instance FromText UserPoolMFAType where
  parser =
    takeLowerText >>= \case
      "on" -> pure UPMTON
      "off" -> pure UPMTOff
      "optional" -> pure UPMTOptional
      e ->
        fromTextError $
          "Failure parsing UserPoolMFAType from value: '" <> e
            <> "'. Accepted values: on, off, optional"

instance ToText UserPoolMFAType where
  toText = \case
    UPMTON -> "ON"
    UPMTOff -> "OFF"
    UPMTOptional -> "OPTIONAL"

instance Hashable UserPoolMFAType

instance NFData UserPoolMFAType

instance ToByteString UserPoolMFAType

instance ToQuery UserPoolMFAType

instance ToHeader UserPoolMFAType

instance ToJSON UserPoolMFAType where
  toJSON = toJSONText

instance FromJSON UserPoolMFAType where
  parseJSON = parseJSONText "UserPoolMFAType"
