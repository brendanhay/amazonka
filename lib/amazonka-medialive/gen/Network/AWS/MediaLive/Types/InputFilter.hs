{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputFilter where

import Network.AWS.Prelude

-- | Input Filter
data InputFilter
  = IFAuto
  | IFDisabled
  | IFForced
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

instance FromText InputFilter where
  parser =
    takeLowerText >>= \case
      "auto" -> pure IFAuto
      "disabled" -> pure IFDisabled
      "forced" -> pure IFForced
      e ->
        fromTextError $
          "Failure parsing InputFilter from value: '" <> e
            <> "'. Accepted values: auto, disabled, forced"

instance ToText InputFilter where
  toText = \case
    IFAuto -> "AUTO"
    IFDisabled -> "DISABLED"
    IFForced -> "FORCED"

instance Hashable InputFilter

instance NFData InputFilter

instance ToByteString InputFilter

instance ToQuery InputFilter

instance ToHeader InputFilter

instance ToJSON InputFilter where
  toJSON = toJSONText

instance FromJSON InputFilter where
  parseJSON = parseJSONText "InputFilter"
