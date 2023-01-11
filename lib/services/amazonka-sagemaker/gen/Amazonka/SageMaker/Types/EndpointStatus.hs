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
-- Module      : Amazonka.SageMaker.Types.EndpointStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointStatus
  ( EndpointStatus
      ( ..,
        EndpointStatus_Creating,
        EndpointStatus_Deleting,
        EndpointStatus_Failed,
        EndpointStatus_InService,
        EndpointStatus_OutOfService,
        EndpointStatus_RollingBack,
        EndpointStatus_SystemUpdating,
        EndpointStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndpointStatus = EndpointStatus'
  { fromEndpointStatus ::
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

pattern EndpointStatus_Creating :: EndpointStatus
pattern EndpointStatus_Creating = EndpointStatus' "Creating"

pattern EndpointStatus_Deleting :: EndpointStatus
pattern EndpointStatus_Deleting = EndpointStatus' "Deleting"

pattern EndpointStatus_Failed :: EndpointStatus
pattern EndpointStatus_Failed = EndpointStatus' "Failed"

pattern EndpointStatus_InService :: EndpointStatus
pattern EndpointStatus_InService = EndpointStatus' "InService"

pattern EndpointStatus_OutOfService :: EndpointStatus
pattern EndpointStatus_OutOfService = EndpointStatus' "OutOfService"

pattern EndpointStatus_RollingBack :: EndpointStatus
pattern EndpointStatus_RollingBack = EndpointStatus' "RollingBack"

pattern EndpointStatus_SystemUpdating :: EndpointStatus
pattern EndpointStatus_SystemUpdating = EndpointStatus' "SystemUpdating"

pattern EndpointStatus_Updating :: EndpointStatus
pattern EndpointStatus_Updating = EndpointStatus' "Updating"

{-# COMPLETE
  EndpointStatus_Creating,
  EndpointStatus_Deleting,
  EndpointStatus_Failed,
  EndpointStatus_InService,
  EndpointStatus_OutOfService,
  EndpointStatus_RollingBack,
  EndpointStatus_SystemUpdating,
  EndpointStatus_Updating,
  EndpointStatus'
  #-}
