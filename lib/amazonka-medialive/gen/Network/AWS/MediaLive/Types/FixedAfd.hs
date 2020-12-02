{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedAfd where

import Network.AWS.Prelude

-- | Fixed Afd
data FixedAfd
  = Afd0000
  | Afd0010
  | Afd0011
  | Afd0100
  | Afd1000
  | Afd1001
  | Afd1010
  | Afd1011
  | Afd1101
  | Afd1110
  | Afd1111
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

instance FromText FixedAfd where
  parser =
    takeLowerText >>= \case
      "afd_0000" -> pure Afd0000
      "afd_0010" -> pure Afd0010
      "afd_0011" -> pure Afd0011
      "afd_0100" -> pure Afd0100
      "afd_1000" -> pure Afd1000
      "afd_1001" -> pure Afd1001
      "afd_1010" -> pure Afd1010
      "afd_1011" -> pure Afd1011
      "afd_1101" -> pure Afd1101
      "afd_1110" -> pure Afd1110
      "afd_1111" -> pure Afd1111
      e ->
        fromTextError $
          "Failure parsing FixedAfd from value: '" <> e
            <> "'. Accepted values: afd_0000, afd_0010, afd_0011, afd_0100, afd_1000, afd_1001, afd_1010, afd_1011, afd_1101, afd_1110, afd_1111"

instance ToText FixedAfd where
  toText = \case
    Afd0000 -> "AFD_0000"
    Afd0010 -> "AFD_0010"
    Afd0011 -> "AFD_0011"
    Afd0100 -> "AFD_0100"
    Afd1000 -> "AFD_1000"
    Afd1001 -> "AFD_1001"
    Afd1010 -> "AFD_1010"
    Afd1011 -> "AFD_1011"
    Afd1101 -> "AFD_1101"
    Afd1110 -> "AFD_1110"
    Afd1111 -> "AFD_1111"

instance Hashable FixedAfd

instance NFData FixedAfd

instance ToByteString FixedAfd

instance ToQuery FixedAfd

instance ToHeader FixedAfd

instance ToJSON FixedAfd where
  toJSON = toJSONText

instance FromJSON FixedAfd where
  parseJSON = parseJSONText "FixedAfd"
