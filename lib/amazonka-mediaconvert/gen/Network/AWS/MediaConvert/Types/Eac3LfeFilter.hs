{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3LfeFilter where

import Network.AWS.Prelude

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
data Eac3LfeFilter
  = ELFDisabled
  | ELFEnabled
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

instance FromText Eac3LfeFilter where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ELFDisabled
      "enabled" -> pure ELFEnabled
      e ->
        fromTextError $
          "Failure parsing Eac3LfeFilter from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Eac3LfeFilter where
  toText = \case
    ELFDisabled -> "DISABLED"
    ELFEnabled -> "ENABLED"

instance Hashable Eac3LfeFilter

instance NFData Eac3LfeFilter

instance ToByteString Eac3LfeFilter

instance ToQuery Eac3LfeFilter

instance ToHeader Eac3LfeFilter

instance ToJSON Eac3LfeFilter where
  toJSON = toJSONText

instance FromJSON Eac3LfeFilter where
  parseJSON = parseJSONText "Eac3LfeFilter"
