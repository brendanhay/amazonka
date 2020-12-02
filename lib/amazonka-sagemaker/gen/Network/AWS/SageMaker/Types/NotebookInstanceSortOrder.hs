{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSortOrder where

import Network.AWS.Prelude

data NotebookInstanceSortOrder
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

instance FromText NotebookInstanceSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure Ascending
      "descending" -> pure Descending
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceSortOrder where
  toText = \case
    Ascending -> "Ascending"
    Descending -> "Descending"

instance Hashable NotebookInstanceSortOrder

instance NFData NotebookInstanceSortOrder

instance ToByteString NotebookInstanceSortOrder

instance ToQuery NotebookInstanceSortOrder

instance ToHeader NotebookInstanceSortOrder

instance ToJSON NotebookInstanceSortOrder where
  toJSON = toJSONText
