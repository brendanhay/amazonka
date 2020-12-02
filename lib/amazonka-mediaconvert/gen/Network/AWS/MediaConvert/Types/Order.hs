{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Order
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Order where

import Network.AWS.Prelude

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
data Order
  = Ascending
  | Descending
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

instance FromText Order where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure Ascending
      "descending" -> pure Descending
      e ->
        fromTextError $
          "Failure parsing Order from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText Order where
  toText = \case
    Ascending -> "ASCENDING"
    Descending -> "DESCENDING"

instance Hashable Order

instance NFData Order

instance ToByteString Order

instance ToQuery Order

instance ToHeader Order

instance ToJSON Order where
  toJSON = toJSONText
