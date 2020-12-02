{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovPaddingControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovPaddingControl where

import Network.AWS.Prelude

-- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
data MovPaddingControl
  = MPCNone
  | MPCOmneon
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

instance FromText MovPaddingControl where
  parser =
    takeLowerText >>= \case
      "none" -> pure MPCNone
      "omneon" -> pure MPCOmneon
      e ->
        fromTextError $
          "Failure parsing MovPaddingControl from value: '" <> e
            <> "'. Accepted values: none, omneon"

instance ToText MovPaddingControl where
  toText = \case
    MPCNone -> "NONE"
    MPCOmneon -> "OMNEON"

instance Hashable MovPaddingControl

instance NFData MovPaddingControl

instance ToByteString MovPaddingControl

instance ToQuery MovPaddingControl

instance ToHeader MovPaddingControl

instance ToJSON MovPaddingControl where
  toJSON = toJSONText

instance FromJSON MovPaddingControl where
  parseJSON = parseJSONText "MovPaddingControl"
