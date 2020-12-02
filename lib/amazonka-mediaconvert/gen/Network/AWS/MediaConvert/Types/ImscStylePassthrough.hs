{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscStylePassthrough
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImscStylePassthrough where

import Network.AWS.Prelude

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
data ImscStylePassthrough
  = ISPDisabled
  | ISPEnabled
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

instance FromText ImscStylePassthrough where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ISPDisabled
      "enabled" -> pure ISPEnabled
      e ->
        fromTextError $
          "Failure parsing ImscStylePassthrough from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ImscStylePassthrough where
  toText = \case
    ISPDisabled -> "DISABLED"
    ISPEnabled -> "ENABLED"

instance Hashable ImscStylePassthrough

instance NFData ImscStylePassthrough

instance ToByteString ImscStylePassthrough

instance ToQuery ImscStylePassthrough

instance ToHeader ImscStylePassthrough

instance ToJSON ImscStylePassthrough where
  toJSON = toJSONText

instance FromJSON ImscStylePassthrough where
  parseJSON = parseJSONText "ImscStylePassthrough"
