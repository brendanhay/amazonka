{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSortOrder where

import Network.AWS.Prelude

data AutoMLSortOrder
  = AMLSOAscending
  | AMLSODescending
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

instance FromText AutoMLSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure AMLSOAscending
      "descending" -> pure AMLSODescending
      e ->
        fromTextError $
          "Failure parsing AutoMLSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText AutoMLSortOrder where
  toText = \case
    AMLSOAscending -> "Ascending"
    AMLSODescending -> "Descending"

instance Hashable AutoMLSortOrder

instance NFData AutoMLSortOrder

instance ToByteString AutoMLSortOrder

instance ToQuery AutoMLSortOrder

instance ToHeader AutoMLSortOrder

instance ToJSON AutoMLSortOrder where
  toJSON = toJSONText
