{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
  ( ProvisionedProductPlanStatus
      ( ..,
        ProvisionedProductPlanStatus_CREATE_FAILED,
        ProvisionedProductPlanStatus_CREATE_IN_PROGRESS,
        ProvisionedProductPlanStatus_CREATE_SUCCESS,
        ProvisionedProductPlanStatus_EXECUTE_FAILED,
        ProvisionedProductPlanStatus_EXECUTE_IN_PROGRESS,
        ProvisionedProductPlanStatus_EXECUTE_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus'
  { fromProvisionedProductPlanStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ProvisionedProductPlanStatus_CREATE_FAILED :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_CREATE_FAILED = ProvisionedProductPlanStatus' "CREATE_FAILED"

pattern ProvisionedProductPlanStatus_CREATE_IN_PROGRESS :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_CREATE_IN_PROGRESS = ProvisionedProductPlanStatus' "CREATE_IN_PROGRESS"

pattern ProvisionedProductPlanStatus_CREATE_SUCCESS :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_CREATE_SUCCESS = ProvisionedProductPlanStatus' "CREATE_SUCCESS"

pattern ProvisionedProductPlanStatus_EXECUTE_FAILED :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_EXECUTE_FAILED = ProvisionedProductPlanStatus' "EXECUTE_FAILED"

pattern ProvisionedProductPlanStatus_EXECUTE_IN_PROGRESS :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_EXECUTE_IN_PROGRESS = ProvisionedProductPlanStatus' "EXECUTE_IN_PROGRESS"

pattern ProvisionedProductPlanStatus_EXECUTE_SUCCESS :: ProvisionedProductPlanStatus
pattern ProvisionedProductPlanStatus_EXECUTE_SUCCESS = ProvisionedProductPlanStatus' "EXECUTE_SUCCESS"

{-# COMPLETE
  ProvisionedProductPlanStatus_CREATE_FAILED,
  ProvisionedProductPlanStatus_CREATE_IN_PROGRESS,
  ProvisionedProductPlanStatus_CREATE_SUCCESS,
  ProvisionedProductPlanStatus_EXECUTE_FAILED,
  ProvisionedProductPlanStatus_EXECUTE_IN_PROGRESS,
  ProvisionedProductPlanStatus_EXECUTE_SUCCESS,
  ProvisionedProductPlanStatus'
  #-}
