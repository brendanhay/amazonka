{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointStatus
  ( EndpointStatus
    ( EndpointStatus'
    , EndpointStatusOutOfService
    , EndpointStatusCreating
    , EndpointStatusUpdating
    , EndpointStatusSystemUpdating
    , EndpointStatusRollingBack
    , EndpointStatusInService
    , EndpointStatusDeleting
    , EndpointStatusFailed
    , fromEndpointStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EndpointStatus = EndpointStatus'{fromEndpointStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern EndpointStatusOutOfService :: EndpointStatus
pattern EndpointStatusOutOfService = EndpointStatus' "OutOfService"

pattern EndpointStatusCreating :: EndpointStatus
pattern EndpointStatusCreating = EndpointStatus' "Creating"

pattern EndpointStatusUpdating :: EndpointStatus
pattern EndpointStatusUpdating = EndpointStatus' "Updating"

pattern EndpointStatusSystemUpdating :: EndpointStatus
pattern EndpointStatusSystemUpdating = EndpointStatus' "SystemUpdating"

pattern EndpointStatusRollingBack :: EndpointStatus
pattern EndpointStatusRollingBack = EndpointStatus' "RollingBack"

pattern EndpointStatusInService :: EndpointStatus
pattern EndpointStatusInService = EndpointStatus' "InService"

pattern EndpointStatusDeleting :: EndpointStatus
pattern EndpointStatusDeleting = EndpointStatus' "Deleting"

pattern EndpointStatusFailed :: EndpointStatus
pattern EndpointStatusFailed = EndpointStatus' "Failed"

{-# COMPLETE 
  EndpointStatusOutOfService,

  EndpointStatusCreating,

  EndpointStatusUpdating,

  EndpointStatusSystemUpdating,

  EndpointStatusRollingBack,

  EndpointStatusInService,

  EndpointStatusDeleting,

  EndpointStatusFailed,
  EndpointStatus'
  #-}
