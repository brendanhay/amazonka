{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsArib
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsArib where

import Network.AWS.Prelude

-- | M2ts Arib
data M2tsArib
  = MADisabled
  | MAEnabled
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

instance FromText M2tsArib where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MADisabled
      "enabled" -> pure MAEnabled
      e ->
        fromTextError $
          "Failure parsing M2tsArib from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText M2tsArib where
  toText = \case
    MADisabled -> "DISABLED"
    MAEnabled -> "ENABLED"

instance Hashable M2tsArib

instance NFData M2tsArib

instance ToByteString M2tsArib

instance ToQuery M2tsArib

instance ToHeader M2tsArib

instance ToJSON M2tsArib where
  toJSON = toJSONText

instance FromJSON M2tsArib where
  parseJSON = parseJSONText "M2tsArib"
