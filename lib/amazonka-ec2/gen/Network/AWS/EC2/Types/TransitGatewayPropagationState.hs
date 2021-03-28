{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayPropagationState
  ( TransitGatewayPropagationState
    ( TransitGatewayPropagationState'
    , TransitGatewayPropagationStateEnabling
    , TransitGatewayPropagationStateEnabled
    , TransitGatewayPropagationStateDisabling
    , TransitGatewayPropagationStateDisabled
    , fromTransitGatewayPropagationState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TransitGatewayPropagationState = TransitGatewayPropagationState'{fromTransitGatewayPropagationState
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

pattern TransitGatewayPropagationStateEnabling :: TransitGatewayPropagationState
pattern TransitGatewayPropagationStateEnabling = TransitGatewayPropagationState' "enabling"

pattern TransitGatewayPropagationStateEnabled :: TransitGatewayPropagationState
pattern TransitGatewayPropagationStateEnabled = TransitGatewayPropagationState' "enabled"

pattern TransitGatewayPropagationStateDisabling :: TransitGatewayPropagationState
pattern TransitGatewayPropagationStateDisabling = TransitGatewayPropagationState' "disabling"

pattern TransitGatewayPropagationStateDisabled :: TransitGatewayPropagationState
pattern TransitGatewayPropagationStateDisabled = TransitGatewayPropagationState' "disabled"

{-# COMPLETE 
  TransitGatewayPropagationStateEnabling,

  TransitGatewayPropagationStateEnabled,

  TransitGatewayPropagationStateDisabling,

  TransitGatewayPropagationStateDisabled,
  TransitGatewayPropagationState'
  #-}
