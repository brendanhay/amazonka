{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnablementTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EnablementTypeFilter where

import Network.AWS.Prelude

data EnablementTypeFilter
  = ETFEnabled
  | ETFPending
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

instance FromText EnablementTypeFilter where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure ETFEnabled
      "pending" -> pure ETFPending
      e ->
        fromTextError $
          "Failure parsing EnablementTypeFilter from value: '" <> e
            <> "'. Accepted values: enabled, pending"

instance ToText EnablementTypeFilter where
  toText = \case
    ETFEnabled -> "ENABLED"
    ETFPending -> "PENDING"

instance Hashable EnablementTypeFilter

instance NFData EnablementTypeFilter

instance ToByteString EnablementTypeFilter

instance ToQuery EnablementTypeFilter

instance ToHeader EnablementTypeFilter

instance ToJSON EnablementTypeFilter where
  toJSON = toJSONText
