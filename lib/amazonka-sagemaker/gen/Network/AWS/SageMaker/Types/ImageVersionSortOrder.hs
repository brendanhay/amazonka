{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionSortOrder where

import Network.AWS.Prelude

data ImageVersionSortOrder
  = IVSOAscending
  | IVSODescending
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

instance FromText ImageVersionSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure IVSOAscending
      "descending" -> pure IVSODescending
      e ->
        fromTextError $
          "Failure parsing ImageVersionSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText ImageVersionSortOrder where
  toText = \case
    IVSOAscending -> "ASCENDING"
    IVSODescending -> "DESCENDING"

instance Hashable ImageVersionSortOrder

instance NFData ImageVersionSortOrder

instance ToByteString ImageVersionSortOrder

instance ToQuery ImageVersionSortOrder

instance ToHeader ImageVersionSortOrder

instance ToJSON ImageVersionSortOrder where
  toJSON = toJSONText
