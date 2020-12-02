{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus where

import Network.AWS.Prelude

data ProvisionedProductStatus
  = PPSAvailable
  | PPSError'
  | PPSPlanInProgress
  | PPSTainted
  | PPSUnderChange
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

instance FromText ProvisionedProductStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure PPSAvailable
      "error" -> pure PPSError'
      "plan_in_progress" -> pure PPSPlanInProgress
      "tainted" -> pure PPSTainted
      "under_change" -> pure PPSUnderChange
      e ->
        fromTextError $
          "Failure parsing ProvisionedProductStatus from value: '" <> e
            <> "'. Accepted values: available, error, plan_in_progress, tainted, under_change"

instance ToText ProvisionedProductStatus where
  toText = \case
    PPSAvailable -> "AVAILABLE"
    PPSError' -> "ERROR"
    PPSPlanInProgress -> "PLAN_IN_PROGRESS"
    PPSTainted -> "TAINTED"
    PPSUnderChange -> "UNDER_CHANGE"

instance Hashable ProvisionedProductStatus

instance NFData ProvisionedProductStatus

instance ToByteString ProvisionedProductStatus

instance ToQuery ProvisionedProductStatus

instance ToHeader ProvisionedProductStatus

instance FromJSON ProvisionedProductStatus where
  parseJSON = parseJSONText "ProvisionedProductStatus"
