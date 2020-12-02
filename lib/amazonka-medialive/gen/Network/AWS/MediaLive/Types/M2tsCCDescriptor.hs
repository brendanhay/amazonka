{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsCCDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsCCDescriptor where

import Network.AWS.Prelude

-- | M2ts Cc Descriptor
data M2tsCCDescriptor
  = MCCDDisabled
  | MCCDEnabled
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

instance FromText M2tsCCDescriptor where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MCCDDisabled
      "enabled" -> pure MCCDEnabled
      e ->
        fromTextError $
          "Failure parsing M2tsCCDescriptor from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText M2tsCCDescriptor where
  toText = \case
    MCCDDisabled -> "DISABLED"
    MCCDEnabled -> "ENABLED"

instance Hashable M2tsCCDescriptor

instance NFData M2tsCCDescriptor

instance ToByteString M2tsCCDescriptor

instance ToQuery M2tsCCDescriptor

instance ToHeader M2tsCCDescriptor

instance ToJSON M2tsCCDescriptor where
  toJSON = toJSONText

instance FromJSON M2tsCCDescriptor where
  parseJSON = parseJSONText "M2tsCCDescriptor"
