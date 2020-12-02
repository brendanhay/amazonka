{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Locale where

import Network.AWS.Prelude

data Locale = EnUs
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

instance FromText Locale where
  parser =
    takeLowerText >>= \case
      "en-us" -> pure EnUs
      e ->
        fromTextError $
          "Failure parsing Locale from value: '" <> e
            <> "'. Accepted values: en-us"

instance ToText Locale where
  toText = \case
    EnUs -> "en-US"

instance Hashable Locale

instance NFData Locale

instance ToByteString Locale

instance ToQuery Locale

instance ToHeader Locale

instance ToJSON Locale where
  toJSON = toJSONText
