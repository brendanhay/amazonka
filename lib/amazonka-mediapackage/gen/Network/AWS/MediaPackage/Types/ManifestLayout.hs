{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.ManifestLayout
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.ManifestLayout where

import Network.AWS.Prelude

data ManifestLayout
  = Compact
  | Full
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

instance FromText ManifestLayout where
  parser =
    takeLowerText >>= \case
      "compact" -> pure Compact
      "full" -> pure Full
      e ->
        fromTextError $
          "Failure parsing ManifestLayout from value: '" <> e
            <> "'. Accepted values: compact, full"

instance ToText ManifestLayout where
  toText = \case
    Compact -> "COMPACT"
    Full -> "FULL"

instance Hashable ManifestLayout

instance NFData ManifestLayout

instance ToByteString ManifestLayout

instance ToQuery ManifestLayout

instance ToHeader ManifestLayout

instance ToJSON ManifestLayout where
  toJSON = toJSONText

instance FromJSON ManifestLayout where
  parseJSON = parseJSONText "ManifestLayout"
