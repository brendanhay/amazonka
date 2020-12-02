{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Syntax where

import Network.AWS.Prelude

-- | Produces a bitstream compliant with SMPTE RP-2027.
data H264Syntax
  = HSDefault
  | HSRP2027
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

instance FromText H264Syntax where
  parser =
    takeLowerText >>= \case
      "default" -> pure HSDefault
      "rp2027" -> pure HSRP2027
      e ->
        fromTextError $
          "Failure parsing H264Syntax from value: '" <> e
            <> "'. Accepted values: default, rp2027"

instance ToText H264Syntax where
  toText = \case
    HSDefault -> "DEFAULT"
    HSRP2027 -> "RP2027"

instance Hashable H264Syntax

instance NFData H264Syntax

instance ToByteString H264Syntax

instance ToQuery H264Syntax

instance ToHeader H264Syntax

instance ToJSON H264Syntax where
  toJSON = toJSONText

instance FromJSON H264Syntax where
  parseJSON = parseJSONText "H264Syntax"
