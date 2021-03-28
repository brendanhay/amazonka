{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
  ( ProvisionedProductPlanStatus
    ( ProvisionedProductPlanStatus'
    , ProvisionedProductPlanStatusCreateInProgress
    , ProvisionedProductPlanStatusCreateSuccess
    , ProvisionedProductPlanStatusCreateFailed
    , ProvisionedProductPlanStatusExecuteInProgress
    , ProvisionedProductPlanStatusExecuteSuccess
    , ProvisionedProductPlanStatusExecuteFailed
    , fromProvisionedProductPlanStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus'{fromProvisionedProductPlanStatus
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern ProvisionedProductPlanStatusCreateInProgress :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusCreateInProgress = ProvisionedProductPlanStatus' "CREATE_IN_PROGRESS"

pattern ProvisionedProductPlanStatusCreateSuccess :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusCreateSuccess = ProvisionedProductPlanStatus' "CREATE_SUCCESS"

pattern ProvisionedProductPlanStatusCreateFailed :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusCreateFailed = ProvisionedProductPlanStatus' "CREATE_FAILED"

pattern ProvisionedProductPlanStatusExecuteInProgress :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusExecuteInProgress = ProvisionedProductPlanStatus' "EXECUTE_IN_PROGRESS"

pattern ProvisionedProductPlanStatusExecuteSuccess :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusExecuteSuccess = ProvisionedProductPlanStatus' "EXECUTE_SUCCESS"

pattern ProvisionedProductPlanStatusExecuteFailed :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatusExecuteFailed = ProvisionedProductPlanStatus' "EXECUTE_FAILED"

{-# COMPLETE 
  ProvisionedProductPlanStatusCreateInProgress,

  ProvisionedProductPlanStatusCreateSuccess,

  ProvisionedProductPlanStatusCreateFailed,

  ProvisionedProductPlanStatusExecuteInProgress,

  ProvisionedProductPlanStatusExecuteSuccess,

  ProvisionedProductPlanStatusExecuteFailed,
  ProvisionedProductPlanStatus'
  #-}
