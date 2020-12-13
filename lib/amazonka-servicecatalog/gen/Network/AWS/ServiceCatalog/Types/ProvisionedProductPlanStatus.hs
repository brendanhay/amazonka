{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
  ( ProvisionedProductPlanStatus
      ( ProvisionedProductPlanStatus',
        CreateInProgress,
        CreateSuccess,
        CreateFailed,
        ExecuteInProgress,
        ExecuteSuccess,
        ExecuteFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CreateInProgress :: ProvisionedProductPlanStatus
pattern CreateInProgress = ProvisionedProductPlanStatus' "CREATE_IN_PROGRESS"

pattern CreateSuccess :: ProvisionedProductPlanStatus
pattern CreateSuccess = ProvisionedProductPlanStatus' "CREATE_SUCCESS"

pattern CreateFailed :: ProvisionedProductPlanStatus
pattern CreateFailed = ProvisionedProductPlanStatus' "CREATE_FAILED"

pattern ExecuteInProgress :: ProvisionedProductPlanStatus
pattern ExecuteInProgress = ProvisionedProductPlanStatus' "EXECUTE_IN_PROGRESS"

pattern ExecuteSuccess :: ProvisionedProductPlanStatus
pattern ExecuteSuccess = ProvisionedProductPlanStatus' "EXECUTE_SUCCESS"

pattern ExecuteFailed :: ProvisionedProductPlanStatus
pattern ExecuteFailed = ProvisionedProductPlanStatus' "EXECUTE_FAILED"

{-# COMPLETE
  CreateInProgress,
  CreateSuccess,
  CreateFailed,
  ExecuteInProgress,
  ExecuteSuccess,
  ExecuteFailed,
  ProvisionedProductPlanStatus'
  #-}
