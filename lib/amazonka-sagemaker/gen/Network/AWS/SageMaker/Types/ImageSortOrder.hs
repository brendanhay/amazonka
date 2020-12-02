{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageSortOrder where

import Network.AWS.Prelude

data ImageSortOrder
  = ISOAscending
  | ISODescending
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

instance FromText ImageSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure ISOAscending
      "descending" -> pure ISODescending
      e ->
        fromTextError $
          "Failure parsing ImageSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText ImageSortOrder where
  toText = \case
    ISOAscending -> "ASCENDING"
    ISODescending -> "DESCENDING"

instance Hashable ImageSortOrder

instance NFData ImageSortOrder

instance ToByteString ImageSortOrder

instance ToQuery ImageSortOrder

instance ToHeader ImageSortOrder

instance ToJSON ImageSortOrder where
  toJSON = toJSONText
