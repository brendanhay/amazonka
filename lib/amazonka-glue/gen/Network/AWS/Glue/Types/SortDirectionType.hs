{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SortDirectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SortDirectionType where

import Network.AWS.Prelude

data SortDirectionType
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

instance FromText SortDirectionType where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure Ascending
      "descending" -> pure Descending
      e ->
        fromTextError $
          "Failure parsing SortDirectionType from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText SortDirectionType where
  toText = \case
    Ascending -> "ASCENDING"
    Descending -> "DESCENDING"

instance Hashable SortDirectionType

instance NFData SortDirectionType

instance ToByteString SortDirectionType

instance ToQuery SortDirectionType

instance ToHeader SortDirectionType

instance ToJSON SortDirectionType where
  toJSON = toJSONText
