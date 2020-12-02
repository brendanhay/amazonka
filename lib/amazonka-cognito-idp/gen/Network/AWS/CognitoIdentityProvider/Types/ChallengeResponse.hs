{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse where

import Network.AWS.Prelude

data ChallengeResponse
  = CFailure
  | CSuccess
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

instance FromText ChallengeResponse where
  parser =
    takeLowerText >>= \case
      "failure" -> pure CFailure
      "success" -> pure CSuccess
      e ->
        fromTextError $
          "Failure parsing ChallengeResponse from value: '" <> e
            <> "'. Accepted values: failure, success"

instance ToText ChallengeResponse where
  toText = \case
    CFailure -> "Failure"
    CSuccess -> "Success"

instance Hashable ChallengeResponse

instance NFData ChallengeResponse

instance ToByteString ChallengeResponse

instance ToQuery ChallengeResponse

instance ToHeader ChallengeResponse

instance FromJSON ChallengeResponse where
  parseJSON = parseJSONText "ChallengeResponse"
