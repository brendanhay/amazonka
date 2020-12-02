{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Owner where

import Network.AWS.Prelude

data Owner
  = AWS
  | CustomLambda
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

instance FromText Owner where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "custom_lambda" -> pure CustomLambda
      e ->
        fromTextError $
          "Failure parsing Owner from value: '" <> e
            <> "'. Accepted values: aws, custom_lambda"

instance ToText Owner where
  toText = \case
    AWS -> "AWS"
    CustomLambda -> "CUSTOM_LAMBDA"

instance Hashable Owner

instance NFData Owner

instance ToByteString Owner

instance ToQuery Owner

instance ToHeader Owner

instance ToJSON Owner where
  toJSON = toJSONText

instance FromJSON Owner where
  parseJSON = parseJSONText "Owner"
