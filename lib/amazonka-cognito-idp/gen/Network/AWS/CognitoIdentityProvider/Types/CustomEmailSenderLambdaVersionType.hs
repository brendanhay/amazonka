{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType where

import Network.AWS.Prelude

data CustomEmailSenderLambdaVersionType = CESLVTV10
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

instance FromText CustomEmailSenderLambdaVersionType where
  parser =
    takeLowerText >>= \case
      "v1_0" -> pure CESLVTV10
      e ->
        fromTextError $
          "Failure parsing CustomEmailSenderLambdaVersionType from value: '" <> e
            <> "'. Accepted values: v1_0"

instance ToText CustomEmailSenderLambdaVersionType where
  toText = \case
    CESLVTV10 -> "V1_0"

instance Hashable CustomEmailSenderLambdaVersionType

instance NFData CustomEmailSenderLambdaVersionType

instance ToByteString CustomEmailSenderLambdaVersionType

instance ToQuery CustomEmailSenderLambdaVersionType

instance ToHeader CustomEmailSenderLambdaVersionType

instance ToJSON CustomEmailSenderLambdaVersionType where
  toJSON = toJSONText

instance FromJSON CustomEmailSenderLambdaVersionType where
  parseJSON = parseJSONText "CustomEmailSenderLambdaVersionType"
