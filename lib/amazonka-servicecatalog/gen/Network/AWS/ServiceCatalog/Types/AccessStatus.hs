{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessStatus where

import Network.AWS.Prelude

data AccessStatus
  = Disabled
  | Enabled
  | UnderChange
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

instance FromText AccessStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      "under_change" -> pure UnderChange
      e ->
        fromTextError $
          "Failure parsing AccessStatus from value: '" <> e
            <> "'. Accepted values: disabled, enabled, under_change"

instance ToText AccessStatus where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"
    UnderChange -> "UNDER_CHANGE"

instance Hashable AccessStatus

instance NFData AccessStatus

instance ToByteString AccessStatus

instance ToQuery AccessStatus

instance ToHeader AccessStatus

instance FromJSON AccessStatus where
  parseJSON = parseJSONText "AccessStatus"
