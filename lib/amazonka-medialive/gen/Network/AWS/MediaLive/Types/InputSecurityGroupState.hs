{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputSecurityGroupState
  ( InputSecurityGroupState
    ( InputSecurityGroupState'
    , InputSecurityGroupStateIdle
    , InputSecurityGroupStateInUse
    , InputSecurityGroupStateUpdating
    , InputSecurityGroupStateDeleted
    , fromInputSecurityGroupState
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for InputSecurityGroupState
newtype InputSecurityGroupState = InputSecurityGroupState'{fromInputSecurityGroupState
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern InputSecurityGroupStateIdle :: InputSecurityGroupState
pattern InputSecurityGroupStateIdle = InputSecurityGroupState' "IDLE"

pattern InputSecurityGroupStateInUse :: InputSecurityGroupState
pattern InputSecurityGroupStateInUse = InputSecurityGroupState' "IN_USE"

pattern InputSecurityGroupStateUpdating :: InputSecurityGroupState
pattern InputSecurityGroupStateUpdating = InputSecurityGroupState' "UPDATING"

pattern InputSecurityGroupStateDeleted :: InputSecurityGroupState
pattern InputSecurityGroupStateDeleted = InputSecurityGroupState' "DELETED"

{-# COMPLETE 
  InputSecurityGroupStateIdle,

  InputSecurityGroupStateInUse,

  InputSecurityGroupStateUpdating,

  InputSecurityGroupStateDeleted,
  InputSecurityGroupState'
  #-}
