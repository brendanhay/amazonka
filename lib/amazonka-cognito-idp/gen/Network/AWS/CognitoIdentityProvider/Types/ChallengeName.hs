{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeName where

import Network.AWS.Prelude

data ChallengeName
  = MFA
  | Password
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

instance FromText ChallengeName where
  parser =
    takeLowerText >>= \case
      "mfa" -> pure MFA
      "password" -> pure Password
      e ->
        fromTextError $
          "Failure parsing ChallengeName from value: '" <> e
            <> "'. Accepted values: mfa, password"

instance ToText ChallengeName where
  toText = \case
    MFA -> "Mfa"
    Password -> "Password"

instance Hashable ChallengeName

instance NFData ChallengeName

instance ToByteString ChallengeName

instance ToQuery ChallengeName

instance ToHeader ChallengeName

instance FromJSON ChallengeName where
  parseJSON = parseJSONText "ChallengeName"
