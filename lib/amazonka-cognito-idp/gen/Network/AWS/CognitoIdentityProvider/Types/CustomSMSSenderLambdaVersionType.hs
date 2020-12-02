{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType where

import Network.AWS.Prelude

data CustomSMSSenderLambdaVersionType = V10
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

instance FromText CustomSMSSenderLambdaVersionType where
  parser =
    takeLowerText >>= \case
      "v1_0" -> pure V10
      e ->
        fromTextError $
          "Failure parsing CustomSMSSenderLambdaVersionType from value: '" <> e
            <> "'. Accepted values: v1_0"

instance ToText CustomSMSSenderLambdaVersionType where
  toText = \case
    V10 -> "V1_0"

instance Hashable CustomSMSSenderLambdaVersionType

instance NFData CustomSMSSenderLambdaVersionType

instance ToByteString CustomSMSSenderLambdaVersionType

instance ToQuery CustomSMSSenderLambdaVersionType

instance ToHeader CustomSMSSenderLambdaVersionType

instance ToJSON CustomSMSSenderLambdaVersionType where
  toJSON = toJSONText

instance FromJSON CustomSMSSenderLambdaVersionType where
  parseJSON = parseJSONText "CustomSMSSenderLambdaVersionType"
