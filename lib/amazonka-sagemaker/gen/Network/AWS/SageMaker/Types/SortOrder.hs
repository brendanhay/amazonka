{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortOrder where

import Network.AWS.Prelude

data SortOrder
  = SOAscending
  | SODescending
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

instance FromText SortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure SOAscending
      "descending" -> pure SODescending
      e ->
        fromTextError $
          "Failure parsing SortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
  toText = \case
    SOAscending -> "Ascending"
    SODescending -> "Descending"

instance Hashable SortOrder

instance NFData SortOrder

instance ToByteString SortOrder

instance ToQuery SortOrder

instance ToHeader SortOrder

instance ToJSON SortOrder where
  toJSON = toJSONText
