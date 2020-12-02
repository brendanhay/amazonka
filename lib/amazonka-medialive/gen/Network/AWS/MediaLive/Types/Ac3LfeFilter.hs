{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3LfeFilter where

import Network.AWS.Prelude

-- | Ac3 Lfe Filter
data Ac3LfeFilter
  = ALFDisabled
  | ALFEnabled
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

instance FromText Ac3LfeFilter where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ALFDisabled
      "enabled" -> pure ALFEnabled
      e ->
        fromTextError $
          "Failure parsing Ac3LfeFilter from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Ac3LfeFilter where
  toText = \case
    ALFDisabled -> "DISABLED"
    ALFEnabled -> "ENABLED"

instance Hashable Ac3LfeFilter

instance NFData Ac3LfeFilter

instance ToByteString Ac3LfeFilter

instance ToQuery Ac3LfeFilter

instance ToHeader Ac3LfeFilter

instance ToJSON Ac3LfeFilter where
  toJSON = toJSONText

instance FromJSON Ac3LfeFilter where
  parseJSON = parseJSONText "Ac3LfeFilter"
