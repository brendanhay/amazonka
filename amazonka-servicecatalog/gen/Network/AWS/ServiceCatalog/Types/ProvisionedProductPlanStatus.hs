{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus'
  { fromProvisionedProductPlanStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
