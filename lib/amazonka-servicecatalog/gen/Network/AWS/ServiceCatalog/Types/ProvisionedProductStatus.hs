{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
  ( ProvisionedProductStatus
    ( ProvisionedProductStatus'
    , ProvisionedProductStatusAvailable
    , ProvisionedProductStatusUnderChange
    , ProvisionedProductStatusTainted
    , ProvisionedProductStatusError
    , ProvisionedProductStatusPlanInProgress
    , fromProvisionedProductStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProvisionedProductStatus = ProvisionedProductStatus'{fromProvisionedProductStatus
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern ProvisionedProductStatusAvailable :: ProvisionedProductStatus
pattern ProvisionedProductStatusAvailable = ProvisionedProductStatus' "AVAILABLE"

pattern ProvisionedProductStatusUnderChange :: ProvisionedProductStatus
pattern ProvisionedProductStatusUnderChange = ProvisionedProductStatus' "UNDER_CHANGE"

pattern ProvisionedProductStatusTainted :: ProvisionedProductStatus
pattern ProvisionedProductStatusTainted = ProvisionedProductStatus' "TAINTED"

pattern ProvisionedProductStatusError :: ProvisionedProductStatus
pattern ProvisionedProductStatusError = ProvisionedProductStatus' "ERROR"

pattern ProvisionedProductStatusPlanInProgress :: ProvisionedProductStatus
pattern ProvisionedProductStatusPlanInProgress = ProvisionedProductStatus' "PLAN_IN_PROGRESS"

{-# COMPLETE 
  ProvisionedProductStatusAvailable,

  ProvisionedProductStatusUnderChange,

  ProvisionedProductStatusTainted,

  ProvisionedProductStatusError,

  ProvisionedProductStatusPlanInProgress,
  ProvisionedProductStatus'
  #-}
