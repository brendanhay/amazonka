{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BehaviorOnMXFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BehaviorOnMXFailure
  ( BehaviorOnMXFailure
    ( BehaviorOnMXFailure'
    , BehaviorOnMXFailureUseDefaultValue
    , BehaviorOnMXFailureRejectMessage
    , fromBehaviorOnMXFailure
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BehaviorOnMXFailure = BehaviorOnMXFailure'{fromBehaviorOnMXFailure
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern BehaviorOnMXFailureUseDefaultValue :: BehaviorOnMXFailure
pattern BehaviorOnMXFailureUseDefaultValue = BehaviorOnMXFailure' "UseDefaultValue"

pattern BehaviorOnMXFailureRejectMessage :: BehaviorOnMXFailure
pattern BehaviorOnMXFailureRejectMessage = BehaviorOnMXFailure' "RejectMessage"

{-# COMPLETE 
  BehaviorOnMXFailureUseDefaultValue,

  BehaviorOnMXFailureRejectMessage,
  BehaviorOnMXFailure'
  #-}
