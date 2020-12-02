{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints where

import Network.AWS.Prelude

-- | <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
data MpdAccessibilityCaptionHints
  = MACHExclude
  | MACHInclude
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

instance FromText MpdAccessibilityCaptionHints where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure MACHExclude
      "include" -> pure MACHInclude
      e ->
        fromTextError $
          "Failure parsing MpdAccessibilityCaptionHints from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText MpdAccessibilityCaptionHints where
  toText = \case
    MACHExclude -> "EXCLUDE"
    MACHInclude -> "INCLUDE"

instance Hashable MpdAccessibilityCaptionHints

instance NFData MpdAccessibilityCaptionHints

instance ToByteString MpdAccessibilityCaptionHints

instance ToQuery MpdAccessibilityCaptionHints

instance ToHeader MpdAccessibilityCaptionHints

instance ToJSON MpdAccessibilityCaptionHints where
  toJSON = toJSONText

instance FromJSON MpdAccessibilityCaptionHints where
  parseJSON = parseJSONText "MpdAccessibilityCaptionHints"
