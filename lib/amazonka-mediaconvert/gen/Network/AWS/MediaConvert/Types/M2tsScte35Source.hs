{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsScte35Source where

import Network.AWS.Prelude

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
data M2tsScte35Source
  = MSSNone
  | MSSPassthrough
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

instance FromText M2tsScte35Source where
  parser =
    takeLowerText >>= \case
      "none" -> pure MSSNone
      "passthrough" -> pure MSSPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsScte35Source from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText M2tsScte35Source where
  toText = \case
    MSSNone -> "NONE"
    MSSPassthrough -> "PASSTHROUGH"

instance Hashable M2tsScte35Source

instance NFData M2tsScte35Source

instance ToByteString M2tsScte35Source

instance ToQuery M2tsScte35Source

instance ToHeader M2tsScte35Source

instance ToJSON M2tsScte35Source where
  toJSON = toJSONText

instance FromJSON M2tsScte35Source where
  parseJSON = parseJSONText "M2tsScte35Source"
