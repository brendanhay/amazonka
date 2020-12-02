{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting where

import Network.AWS.Prelude

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
data HlsCaptionLanguageSetting
  = HCLSInsert
  | HCLSNone
  | HCLSOmit
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

instance FromText HlsCaptionLanguageSetting where
  parser =
    takeLowerText >>= \case
      "insert" -> pure HCLSInsert
      "none" -> pure HCLSNone
      "omit" -> pure HCLSOmit
      e ->
        fromTextError $
          "Failure parsing HlsCaptionLanguageSetting from value: '" <> e
            <> "'. Accepted values: insert, none, omit"

instance ToText HlsCaptionLanguageSetting where
  toText = \case
    HCLSInsert -> "INSERT"
    HCLSNone -> "NONE"
    HCLSOmit -> "OMIT"

instance Hashable HlsCaptionLanguageSetting

instance NFData HlsCaptionLanguageSetting

instance ToByteString HlsCaptionLanguageSetting

instance ToQuery HlsCaptionLanguageSetting

instance ToHeader HlsCaptionLanguageSetting

instance ToJSON HlsCaptionLanguageSetting where
  toJSON = toJSONText

instance FromJSON HlsCaptionLanguageSetting where
  parseJSON = parseJSONText "HlsCaptionLanguageSetting"
