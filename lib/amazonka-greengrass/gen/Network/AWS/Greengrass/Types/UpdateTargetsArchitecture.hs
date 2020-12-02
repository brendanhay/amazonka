{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateTargetsArchitecture
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateTargetsArchitecture where

import Network.AWS.Prelude

-- | The architecture of the cores which are the targets of an update.
data UpdateTargetsArchitecture
  = AARCH64
  | Armv6l
  | Armv7l
  | X86_64
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

instance FromText UpdateTargetsArchitecture where
  parser =
    takeLowerText >>= \case
      "aarch64" -> pure AARCH64
      "armv6l" -> pure Armv6l
      "armv7l" -> pure Armv7l
      "x86_64" -> pure X86_64
      e ->
        fromTextError $
          "Failure parsing UpdateTargetsArchitecture from value: '" <> e
            <> "'. Accepted values: aarch64, armv6l, armv7l, x86_64"

instance ToText UpdateTargetsArchitecture where
  toText = \case
    AARCH64 -> "aarch64"
    Armv6l -> "armv6l"
    Armv7l -> "armv7l"
    X86_64 -> "x86_64"

instance Hashable UpdateTargetsArchitecture

instance NFData UpdateTargetsArchitecture

instance ToByteString UpdateTargetsArchitecture

instance ToQuery UpdateTargetsArchitecture

instance ToHeader UpdateTargetsArchitecture

instance ToJSON UpdateTargetsArchitecture where
  toJSON = toJSONText
