{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.HsmState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.HsmState
  ( HsmState
    ( HsmState'
    , HsmStateCreateInProgress
    , HsmStateActive
    , HsmStateDegraded
    , HsmStateDeleteInProgress
    , HsmStateDeleted
    , fromHsmState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HsmState = HsmState'{fromHsmState :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern HsmStateCreateInProgress :: HsmState
pattern HsmStateCreateInProgress = HsmState' "CREATE_IN_PROGRESS"

pattern HsmStateActive :: HsmState
pattern HsmStateActive = HsmState' "ACTIVE"

pattern HsmStateDegraded :: HsmState
pattern HsmStateDegraded = HsmState' "DEGRADED"

pattern HsmStateDeleteInProgress :: HsmState
pattern HsmStateDeleteInProgress = HsmState' "DELETE_IN_PROGRESS"

pattern HsmStateDeleted :: HsmState
pattern HsmStateDeleted = HsmState' "DELETED"

{-# COMPLETE 
  HsmStateCreateInProgress,

  HsmStateActive,

  HsmStateDegraded,

  HsmStateDeleteInProgress,

  HsmStateDeleted,
  HsmState'
  #-}
