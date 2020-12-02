{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl where

import Network.AWS.Prelude

-- | Ebu Tt DFill Line Gap Control
data EbuTtDFillLineGapControl
  = ETDFLGCDisabled
  | ETDFLGCEnabled
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

instance FromText EbuTtDFillLineGapControl where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ETDFLGCDisabled
      "enabled" -> pure ETDFLGCEnabled
      e ->
        fromTextError $
          "Failure parsing EbuTtDFillLineGapControl from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText EbuTtDFillLineGapControl where
  toText = \case
    ETDFLGCDisabled -> "DISABLED"
    ETDFLGCEnabled -> "ENABLED"

instance Hashable EbuTtDFillLineGapControl

instance NFData EbuTtDFillLineGapControl

instance ToByteString EbuTtDFillLineGapControl

instance ToQuery EbuTtDFillLineGapControl

instance ToHeader EbuTtDFillLineGapControl

instance ToJSON EbuTtDFillLineGapControl where
  toJSON = toJSONText

instance FromJSON EbuTtDFillLineGapControl where
  parseJSON = parseJSONText "EbuTtDFillLineGapControl"
