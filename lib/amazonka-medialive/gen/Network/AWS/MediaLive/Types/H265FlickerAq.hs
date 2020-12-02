{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FlickerAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FlickerAq where

import Network.AWS.Prelude

-- | H265 Flicker Aq
data H265FlickerAq
  = HFADisabled
  | HFAEnabled
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

instance FromText H265FlickerAq where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HFADisabled
      "enabled" -> pure HFAEnabled
      e ->
        fromTextError $
          "Failure parsing H265FlickerAq from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265FlickerAq where
  toText = \case
    HFADisabled -> "DISABLED"
    HFAEnabled -> "ENABLED"

instance Hashable H265FlickerAq

instance NFData H265FlickerAq

instance ToByteString H265FlickerAq

instance ToQuery H265FlickerAq

instance ToHeader H265FlickerAq

instance ToJSON H265FlickerAq where
  toJSON = toJSONText

instance FromJSON H265FlickerAq where
  parseJSON = parseJSONText "H265FlickerAq"
