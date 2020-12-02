{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.SortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.SortOrder where

import Network.AWS.Prelude

-- | The sort order specified in a listing condition. Possible values include the following:
--
--
--     * @asc@ - Present the information in ascending order (from A-Z).    * @dsc@ - Present the information in descending order (from Z-A).
data SortOrder
  = Asc
  | Dsc
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
      "asc" -> pure Asc
      "dsc" -> pure Dsc
      e ->
        fromTextError $
          "Failure parsing SortOrder from value: '" <> e
            <> "'. Accepted values: asc, dsc"

instance ToText SortOrder where
  toText = \case
    Asc -> "asc"
    Dsc -> "dsc"

instance Hashable SortOrder

instance NFData SortOrder

instance ToByteString SortOrder

instance ToQuery SortOrder

instance ToHeader SortOrder

instance ToJSON SortOrder where
  toJSON = toJSONText
