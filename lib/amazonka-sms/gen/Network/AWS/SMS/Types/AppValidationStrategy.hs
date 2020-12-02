{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationStrategy where

import Network.AWS.Prelude

data AppValidationStrategy = Ssm
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

instance FromText AppValidationStrategy where
  parser =
    takeLowerText >>= \case
      "ssm" -> pure Ssm
      e ->
        fromTextError $
          "Failure parsing AppValidationStrategy from value: '" <> e
            <> "'. Accepted values: ssm"

instance ToText AppValidationStrategy where
  toText = \case
    Ssm -> "SSM"

instance Hashable AppValidationStrategy

instance NFData AppValidationStrategy

instance ToByteString AppValidationStrategy

instance ToQuery AppValidationStrategy

instance ToHeader AppValidationStrategy

instance ToJSON AppValidationStrategy where
  toJSON = toJSONText

instance FromJSON AppValidationStrategy where
  parseJSON = parseJSONText "AppValidationStrategy"
