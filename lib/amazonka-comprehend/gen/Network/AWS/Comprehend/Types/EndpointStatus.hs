{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EndpointStatus
  ( EndpointStatus
    ( EndpointStatus'
    , EndpointStatusCreating
    , EndpointStatusDeleting
    , EndpointStatusFailed
    , EndpointStatusInService
    , EndpointStatusUpdating
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

pattern EndpointStatusCreating :: EndpointStatus
pattern EndpointStatusCreating = EndpointStatus' "CREATING"

pattern EndpointStatusDeleting :: EndpointStatus
pattern EndpointStatusDeleting = EndpointStatus' "DELETING"

pattern EndpointStatusFailed :: EndpointStatus
pattern EndpointStatusFailed = EndpointStatus' "FAILED"

pattern EndpointStatusInService :: EndpointStatus
pattern EndpointStatusInService = EndpointStatus' "IN_SERVICE"

pattern EndpointStatusUpdating :: EndpointStatus
pattern EndpointStatusUpdating = EndpointStatus' "UPDATING"

{-# COMPLETE 
  EndpointStatusCreating,

  EndpointStatusDeleting,

  EndpointStatusFailed,

  EndpointStatusInService,

  EndpointStatusUpdating,
  EndpointStatus'
  #-}
