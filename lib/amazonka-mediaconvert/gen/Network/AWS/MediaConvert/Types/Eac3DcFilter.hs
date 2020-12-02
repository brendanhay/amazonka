{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DcFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DcFilter where

import Network.AWS.Prelude

-- | Activates a DC highpass filter for all input channels.
data Eac3DcFilter
  = EDFDisabled
  | EDFEnabled
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

instance FromText Eac3DcFilter where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure EDFDisabled
      "enabled" -> pure EDFEnabled
      e ->
        fromTextError $
          "Failure parsing Eac3DcFilter from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Eac3DcFilter where
  toText = \case
    EDFDisabled -> "DISABLED"
    EDFEnabled -> "ENABLED"

instance Hashable Eac3DcFilter

instance NFData Eac3DcFilter

instance ToByteString Eac3DcFilter

instance ToQuery Eac3DcFilter

instance ToHeader Eac3DcFilter

instance ToJSON Eac3DcFilter where
  toJSON = toJSONText

instance FromJSON Eac3DcFilter where
  parseJSON = parseJSONText "Eac3DcFilter"
