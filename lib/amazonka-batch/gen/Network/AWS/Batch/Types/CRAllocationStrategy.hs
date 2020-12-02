{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CRAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CRAllocationStrategy where

import Network.AWS.Prelude

data CRAllocationStrategy
  = BestFit
  | BestFitProgressive
  | SpotCapacityOptimized
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

instance FromText CRAllocationStrategy where
  parser =
    takeLowerText >>= \case
      "best_fit" -> pure BestFit
      "best_fit_progressive" -> pure BestFitProgressive
      "spot_capacity_optimized" -> pure SpotCapacityOptimized
      e ->
        fromTextError $
          "Failure parsing CRAllocationStrategy from value: '" <> e
            <> "'. Accepted values: best_fit, best_fit_progressive, spot_capacity_optimized"

instance ToText CRAllocationStrategy where
  toText = \case
    BestFit -> "BEST_FIT"
    BestFitProgressive -> "BEST_FIT_PROGRESSIVE"
    SpotCapacityOptimized -> "SPOT_CAPACITY_OPTIMIZED"

instance Hashable CRAllocationStrategy

instance NFData CRAllocationStrategy

instance ToByteString CRAllocationStrategy

instance ToQuery CRAllocationStrategy

instance ToHeader CRAllocationStrategy

instance ToJSON CRAllocationStrategy where
  toJSON = toJSONText

instance FromJSON CRAllocationStrategy where
  parseJSON = parseJSONText "CRAllocationStrategy"
