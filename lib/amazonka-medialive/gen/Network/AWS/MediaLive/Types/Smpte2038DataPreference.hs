{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Smpte2038DataPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Smpte2038DataPreference where

import Network.AWS.Prelude

-- | Smpte2038 Data Preference
data Smpte2038DataPreference
  = Ignore
  | Prefer
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

instance FromText Smpte2038DataPreference where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure Ignore
      "prefer" -> pure Prefer
      e ->
        fromTextError $
          "Failure parsing Smpte2038DataPreference from value: '" <> e
            <> "'. Accepted values: ignore, prefer"

instance ToText Smpte2038DataPreference where
  toText = \case
    Ignore -> "IGNORE"
    Prefer -> "PREFER"

instance Hashable Smpte2038DataPreference

instance NFData Smpte2038DataPreference

instance ToByteString Smpte2038DataPreference

instance ToQuery Smpte2038DataPreference

instance ToHeader Smpte2038DataPreference

instance ToJSON Smpte2038DataPreference where
  toJSON = toJSONText

instance FromJSON Smpte2038DataPreference where
  parseJSON = parseJSONText "Smpte2038DataPreference"
