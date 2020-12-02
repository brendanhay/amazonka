{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.PeriodTriggersElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.PeriodTriggersElement where

import Network.AWS.Prelude

data PeriodTriggersElement = Ads
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

instance FromText PeriodTriggersElement where
  parser =
    takeLowerText >>= \case
      "ads" -> pure Ads
      e ->
        fromTextError $
          "Failure parsing PeriodTriggersElement from value: '" <> e
            <> "'. Accepted values: ads"

instance ToText PeriodTriggersElement where
  toText = \case
    Ads -> "ADS"

instance Hashable PeriodTriggersElement

instance NFData PeriodTriggersElement

instance ToByteString PeriodTriggersElement

instance ToQuery PeriodTriggersElement

instance ToHeader PeriodTriggersElement

instance ToJSON PeriodTriggersElement where
  toJSON = toJSONText

instance FromJSON PeriodTriggersElement where
  parseJSON = parseJSONText "PeriodTriggersElement"
