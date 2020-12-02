{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder where

import Network.AWS.Prelude

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
data M2tsForceTsVideoEbpOrder
  = MFTVEODefault
  | MFTVEOForce
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

instance FromText M2tsForceTsVideoEbpOrder where
  parser =
    takeLowerText >>= \case
      "default" -> pure MFTVEODefault
      "force" -> pure MFTVEOForce
      e ->
        fromTextError $
          "Failure parsing M2tsForceTsVideoEbpOrder from value: '" <> e
            <> "'. Accepted values: default, force"

instance ToText M2tsForceTsVideoEbpOrder where
  toText = \case
    MFTVEODefault -> "DEFAULT"
    MFTVEOForce -> "FORCE"

instance Hashable M2tsForceTsVideoEbpOrder

instance NFData M2tsForceTsVideoEbpOrder

instance ToByteString M2tsForceTsVideoEbpOrder

instance ToQuery M2tsForceTsVideoEbpOrder

instance ToHeader M2tsForceTsVideoEbpOrder

instance ToJSON M2tsForceTsVideoEbpOrder where
  toJSON = toJSONText

instance FromJSON M2tsForceTsVideoEbpOrder where
  parseJSON = parseJSONText "M2tsForceTsVideoEbpOrder"
