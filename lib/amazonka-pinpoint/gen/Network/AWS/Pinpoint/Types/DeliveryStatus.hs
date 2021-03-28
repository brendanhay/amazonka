{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DeliveryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DeliveryStatus
  ( DeliveryStatus
    ( DeliveryStatus'
    , DeliveryStatusSuccessful
    , DeliveryStatusThrottled
    , DeliveryStatusTemporaryFailure
    , DeliveryStatusPermanentFailure
    , DeliveryStatusUnknownFailure
    , DeliveryStatusOptOut
    , DeliveryStatusDuplicate
    , fromDeliveryStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeliveryStatus = DeliveryStatus'{fromDeliveryStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern DeliveryStatusSuccessful :: DeliveryStatus
pattern DeliveryStatusSuccessful = DeliveryStatus' "SUCCESSFUL"

pattern DeliveryStatusThrottled :: DeliveryStatus
pattern DeliveryStatusThrottled = DeliveryStatus' "THROTTLED"

pattern DeliveryStatusTemporaryFailure :: DeliveryStatus
pattern DeliveryStatusTemporaryFailure = DeliveryStatus' "TEMPORARY_FAILURE"

pattern DeliveryStatusPermanentFailure :: DeliveryStatus
pattern DeliveryStatusPermanentFailure = DeliveryStatus' "PERMANENT_FAILURE"

pattern DeliveryStatusUnknownFailure :: DeliveryStatus
pattern DeliveryStatusUnknownFailure = DeliveryStatus' "UNKNOWN_FAILURE"

pattern DeliveryStatusOptOut :: DeliveryStatus
pattern DeliveryStatusOptOut = DeliveryStatus' "OPT_OUT"

pattern DeliveryStatusDuplicate :: DeliveryStatus
pattern DeliveryStatusDuplicate = DeliveryStatus' "DUPLICATE"

{-# COMPLETE 
  DeliveryStatusSuccessful,

  DeliveryStatusThrottled,

  DeliveryStatusTemporaryFailure,

  DeliveryStatusPermanentFailure,

  DeliveryStatusUnknownFailure,

  DeliveryStatusOptOut,

  DeliveryStatusDuplicate,
  DeliveryStatus'
  #-}
