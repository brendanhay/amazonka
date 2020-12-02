{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryStatus where

import Network.AWS.Prelude

data CostCategoryStatus
  = Applied
  | Processing
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

instance FromText CostCategoryStatus where
  parser =
    takeLowerText >>= \case
      "applied" -> pure Applied
      "processing" -> pure Processing
      e ->
        fromTextError $
          "Failure parsing CostCategoryStatus from value: '" <> e
            <> "'. Accepted values: applied, processing"

instance ToText CostCategoryStatus where
  toText = \case
    Applied -> "APPLIED"
    Processing -> "PROCESSING"

instance Hashable CostCategoryStatus

instance NFData CostCategoryStatus

instance ToByteString CostCategoryStatus

instance ToQuery CostCategoryStatus

instance ToHeader CostCategoryStatus

instance FromJSON CostCategoryStatus where
  parseJSON = parseJSONText "CostCategoryStatus"
