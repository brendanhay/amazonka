{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder where

import Network.AWS.Prelude

data NotebookInstanceLifecycleConfigSortOrder
  = NILCSOAscending
  | NILCSODescending
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

instance FromText NotebookInstanceLifecycleConfigSortOrder where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure NILCSOAscending
      "descending" -> pure NILCSODescending
      e ->
        fromTextError $
          "Failure parsing NotebookInstanceLifecycleConfigSortOrder from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText NotebookInstanceLifecycleConfigSortOrder where
  toText = \case
    NILCSOAscending -> "Ascending"
    NILCSODescending -> "Descending"

instance Hashable NotebookInstanceLifecycleConfigSortOrder

instance NFData NotebookInstanceLifecycleConfigSortOrder

instance ToByteString NotebookInstanceLifecycleConfigSortOrder

instance ToQuery NotebookInstanceLifecycleConfigSortOrder

instance ToHeader NotebookInstanceLifecycleConfigSortOrder

instance ToJSON NotebookInstanceLifecycleConfigSortOrder where
  toJSON = toJSONText
