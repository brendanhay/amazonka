{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.SortOrderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.SortOrderType where

import Network.AWS.Prelude

data SortOrderType
  = Asc
  | Desc
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

instance FromText SortOrderType where
  parser =
    takeLowerText >>= \case
      "asc" -> pure Asc
      "desc" -> pure Desc
      e ->
        fromTextError $
          "Failure parsing SortOrderType from value: '" <> e
            <> "'. Accepted values: asc, desc"

instance ToText SortOrderType where
  toText = \case
    Asc -> "asc"
    Desc -> "desc"

instance Hashable SortOrderType

instance NFData SortOrderType

instance ToByteString SortOrderType

instance ToQuery SortOrderType

instance ToHeader SortOrderType

instance ToJSON SortOrderType where
  toJSON = toJSONText
