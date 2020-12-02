{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafClientCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafClientCache where

import Network.AWS.Prelude

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
data CmafClientCache
  = CCCDisabled
  | CCCEnabled
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

instance FromText CmafClientCache where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CCCDisabled
      "enabled" -> pure CCCEnabled
      e ->
        fromTextError $
          "Failure parsing CmafClientCache from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText CmafClientCache where
  toText = \case
    CCCDisabled -> "DISABLED"
    CCCEnabled -> "ENABLED"

instance Hashable CmafClientCache

instance NFData CmafClientCache

instance ToByteString CmafClientCache

instance ToQuery CmafClientCache

instance ToHeader CmafClientCache

instance ToJSON CmafClientCache where
  toJSON = toJSONText

instance FromJSON CmafClientCache where
  parseJSON = parseJSONText "CmafClientCache"
