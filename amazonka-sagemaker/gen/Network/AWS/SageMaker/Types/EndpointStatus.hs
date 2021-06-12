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
-- Module      : Network.AWS.SageMaker.Types.EndpointStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointStatus
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

import qualified Network.AWS.Core as Core

newtype EndpointStatus = EndpointStatus'
  { fromEndpointStatus ::
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
