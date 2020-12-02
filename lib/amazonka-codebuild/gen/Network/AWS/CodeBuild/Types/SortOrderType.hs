{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SortOrderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SortOrderType where

import Network.AWS.Prelude

data SortOrderType
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

instance FromText SortOrderType where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure Ascending
      "descending" -> pure Descending
      e ->
        fromTextError $
          "Failure parsing SortOrderType from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText SortOrderType where
  toText = \case
    Ascending -> "ASCENDING"
    Descending -> "DESCENDING"

instance Hashable SortOrderType

instance NFData SortOrderType

instance ToByteString SortOrderType

instance ToQuery SortOrderType

instance ToHeader SortOrderType

instance ToJSON SortOrderType where
  toJSON = toJSONText
