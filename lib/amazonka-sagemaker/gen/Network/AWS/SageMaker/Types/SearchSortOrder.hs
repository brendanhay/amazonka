{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchSortOrder where

import Network.AWS.Prelude

data SearchSortOrder
  = SSOAscending
  | SSODescending
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

instance FromText SearchSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure SSOAscending
      "descending" -> pure SSODescending
      e ->
        fromTextError $
          "Failure parsing SearchSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText SearchSortOrder where
  toText = \case
    SSOAscending -> "Ascending"
    SSODescending -> "Descending"

instance Hashable SearchSortOrder

instance NFData SearchSortOrder

instance ToByteString SearchSortOrder

instance ToQuery SearchSortOrder

instance ToHeader SearchSortOrder

instance ToJSON SearchSortOrder where
  toJSON = toJSONText
