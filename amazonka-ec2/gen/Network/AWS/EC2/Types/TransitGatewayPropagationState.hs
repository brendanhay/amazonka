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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPropagationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPropagationState
  ( TransitGatewayPropagationState
      ( ..,
        TransitGatewayPropagationState_Disabled,
        TransitGatewayPropagationState_Disabling,
        TransitGatewayPropagationState_Enabled,
        TransitGatewayPropagationState_Enabling
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype TransitGatewayPropagationState = TransitGatewayPropagationState'
  { fromTransitGatewayPropagationState ::
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

pattern TransitGatewayPropagationState_Disabled :: TransitGatewayPropagationState
pattern TransitGatewayPropagationState_Disabled = TransitGatewayPropagationState' "disabled"

pattern TransitGatewayPropagationState_Disabling :: TransitGatewayPropagationState
pattern TransitGatewayPropagationState_Disabling = TransitGatewayPropagationState' "disabling"

pattern TransitGatewayPropagationState_Enabled :: TransitGatewayPropagationState
pattern TransitGatewayPropagationState_Enabled = TransitGatewayPropagationState' "enabled"

pattern TransitGatewayPropagationState_Enabling :: TransitGatewayPropagationState
pattern TransitGatewayPropagationState_Enabling = TransitGatewayPropagationState' "enabling"

{-# COMPLETE
  TransitGatewayPropagationState_Disabled,
  TransitGatewayPropagationState_Disabling,
  TransitGatewayPropagationState_Enabled,
  TransitGatewayPropagationState_Enabling,
  TransitGatewayPropagationState'
  #-}
