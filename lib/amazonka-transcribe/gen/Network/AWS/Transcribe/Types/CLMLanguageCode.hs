{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.CLMLanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.CLMLanguageCode where

import Network.AWS.Prelude

data CLMLanguageCode = CLMLCEnUs
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

instance FromText CLMLanguageCode where
  parser =
    takeLowerText >>= \case
      "en-us" -> pure CLMLCEnUs
      e ->
        fromTextError $
          "Failure parsing CLMLanguageCode from value: '" <> e
            <> "'. Accepted values: en-us"

instance ToText CLMLanguageCode where
  toText = \case
    CLMLCEnUs -> "en-US"

instance Hashable CLMLanguageCode

instance NFData CLMLanguageCode

instance ToByteString CLMLanguageCode

instance ToQuery CLMLanguageCode

instance ToHeader CLMLanguageCode

instance ToJSON CLMLanguageCode where
  toJSON = toJSONText

instance FromJSON CLMLanguageCode where
  parseJSON = parseJSONText "CLMLanguageCode"
