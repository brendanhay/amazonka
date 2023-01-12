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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisionedProductPlanStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProvisionedProductPlanStatus = ProvisionedProductPlanStatus'
  { fromProvisionedProductPlanStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
