{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ScalingStatusType
  ( ScalingStatusType
    ( ScalingStatusType'
    , ScalingStatusTypeActive
    , ScalingStatusTypeUpdateRequested
    , ScalingStatusTypeUpdating
    , ScalingStatusTypeDeleteRequested
    , ScalingStatusTypeDeleting
    , ScalingStatusTypeDeleted
    , ScalingStatusTypeError
    , fromScalingStatusType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ScalingStatusType = ScalingStatusType'{fromScalingStatusType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ScalingStatusTypeActive :: ScalingStatusType
pattern ScalingStatusTypeActive = ScalingStatusType' "ACTIVE"

pattern ScalingStatusTypeUpdateRequested :: ScalingStatusType
pattern ScalingStatusTypeUpdateRequested = ScalingStatusType' "UPDATE_REQUESTED"

pattern ScalingStatusTypeUpdating :: ScalingStatusType
pattern ScalingStatusTypeUpdating = ScalingStatusType' "UPDATING"

pattern ScalingStatusTypeDeleteRequested :: ScalingStatusType
pattern ScalingStatusTypeDeleteRequested = ScalingStatusType' "DELETE_REQUESTED"

pattern ScalingStatusTypeDeleting :: ScalingStatusType
pattern ScalingStatusTypeDeleting = ScalingStatusType' "DELETING"

pattern ScalingStatusTypeDeleted :: ScalingStatusType
pattern ScalingStatusTypeDeleted = ScalingStatusType' "DELETED"

pattern ScalingStatusTypeError :: ScalingStatusType
pattern ScalingStatusTypeError = ScalingStatusType' "ERROR"

{-# COMPLETE 
  ScalingStatusTypeActive,

  ScalingStatusTypeUpdateRequested,

  ScalingStatusTypeUpdating,

  ScalingStatusTypeDeleteRequested,

  ScalingStatusTypeDeleting,

  ScalingStatusTypeDeleted,

  ScalingStatusTypeError,
  ScalingStatusType'
  #-}
