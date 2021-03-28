{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotInstanceRequestState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CancelSpotInstanceRequestState
  ( CancelSpotInstanceRequestState
    ( CancelSpotInstanceRequestState'
    , CancelSpotInstanceRequestStateActive
    , CancelSpotInstanceRequestStateOpen
    , CancelSpotInstanceRequestStateClosed
    , CancelSpotInstanceRequestStateCancelled
    , CancelSpotInstanceRequestStateCompleted
    , fromCancelSpotInstanceRequestState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CancelSpotInstanceRequestState = CancelSpotInstanceRequestState'{fromCancelSpotInstanceRequestState
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern CancelSpotInstanceRequestStateActive :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestStateActive = CancelSpotInstanceRequestState' "active"

pattern CancelSpotInstanceRequestStateOpen :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestStateOpen = CancelSpotInstanceRequestState' "open"

pattern CancelSpotInstanceRequestStateClosed :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestStateClosed = CancelSpotInstanceRequestState' "closed"

pattern CancelSpotInstanceRequestStateCancelled :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestStateCancelled = CancelSpotInstanceRequestState' "cancelled"

pattern CancelSpotInstanceRequestStateCompleted :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestStateCompleted = CancelSpotInstanceRequestState' "completed"

{-# COMPLETE 
  CancelSpotInstanceRequestStateActive,

  CancelSpotInstanceRequestStateOpen,

  CancelSpotInstanceRequestStateClosed,

  CancelSpotInstanceRequestStateCancelled,

  CancelSpotInstanceRequestStateCompleted,
  CancelSpotInstanceRequestState'
  #-}
