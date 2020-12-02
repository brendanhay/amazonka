{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType where

import Network.AWS.Prelude

data VerifySoftwareTokenResponseType
  = VSTRTError'
  | VSTRTSuccess
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

instance FromText VerifySoftwareTokenResponseType where
  parser =
    takeLowerText >>= \case
      "error" -> pure VSTRTError'
      "success" -> pure VSTRTSuccess
      e ->
        fromTextError $
          "Failure parsing VerifySoftwareTokenResponseType from value: '" <> e
            <> "'. Accepted values: error, success"

instance ToText VerifySoftwareTokenResponseType where
  toText = \case
    VSTRTError' -> "ERROR"
    VSTRTSuccess -> "SUCCESS"

instance Hashable VerifySoftwareTokenResponseType

instance NFData VerifySoftwareTokenResponseType

instance ToByteString VerifySoftwareTokenResponseType

instance ToQuery VerifySoftwareTokenResponseType

instance ToHeader VerifySoftwareTokenResponseType

instance FromJSON VerifySoftwareTokenResponseType where
  parseJSON = parseJSONText "VerifySoftwareTokenResponseType"
