{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus where

import Network.AWS.Prelude

data ProvisionedProductPlanStatus
  = CreateFailed
  | CreateInProgress
  | CreateSuccess
  | ExecuteFailed
  | ExecuteInProgress
  | ExecuteSuccess
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

instance FromText ProvisionedProductPlanStatus where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure CreateFailed
      "create_in_progress" -> pure CreateInProgress
      "create_success" -> pure CreateSuccess
      "execute_failed" -> pure ExecuteFailed
      "execute_in_progress" -> pure ExecuteInProgress
      "execute_success" -> pure ExecuteSuccess
      e ->
        fromTextError $
          "Failure parsing ProvisionedProductPlanStatus from value: '" <> e
            <> "'. Accepted values: create_failed, create_in_progress, create_success, execute_failed, execute_in_progress, execute_success"

instance ToText ProvisionedProductPlanStatus where
  toText = \case
    CreateFailed -> "CREATE_FAILED"
    CreateInProgress -> "CREATE_IN_PROGRESS"
    CreateSuccess -> "CREATE_SUCCESS"
    ExecuteFailed -> "EXECUTE_FAILED"
    ExecuteInProgress -> "EXECUTE_IN_PROGRESS"
    ExecuteSuccess -> "EXECUTE_SUCCESS"

instance Hashable ProvisionedProductPlanStatus

instance NFData ProvisionedProductPlanStatus

instance ToByteString ProvisionedProductPlanStatus

instance ToQuery ProvisionedProductPlanStatus

instance ToHeader ProvisionedProductPlanStatus

instance FromJSON ProvisionedProductPlanStatus where
  parseJSON = parseJSONText "ProvisionedProductPlanStatus"
