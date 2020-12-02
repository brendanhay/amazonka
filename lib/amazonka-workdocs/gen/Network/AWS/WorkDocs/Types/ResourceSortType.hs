{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourceSortType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourceSortType where

import Network.AWS.Prelude

data ResourceSortType
  = Date
  | Name
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

instance FromText ResourceSortType where
  parser =
    takeLowerText >>= \case
      "date" -> pure Date
      "name" -> pure Name
      e ->
        fromTextError $
          "Failure parsing ResourceSortType from value: '" <> e
            <> "'. Accepted values: date, name"

instance ToText ResourceSortType where
  toText = \case
    Date -> "DATE"
    Name -> "NAME"

instance Hashable ResourceSortType

instance NFData ResourceSortType

instance ToByteString ResourceSortType

instance ToQuery ResourceSortType

instance ToHeader ResourceSortType

instance ToJSON ResourceSortType where
  toJSON = toJSONText
