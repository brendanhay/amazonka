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
-- Module      : Network.AWS.EC2.Types.TransitGatewayState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayState
  ( TransitGatewayState
      ( ..,
        TransitGatewayState_Available,
        TransitGatewayState_Deleted,
        TransitGatewayState_Deleting,
        TransitGatewayState_Modifying,
        TransitGatewayState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype TransitGatewayState = TransitGatewayState'
  { fromTransitGatewayState ::
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

pattern TransitGatewayState_Available :: TransitGatewayState
pattern TransitGatewayState_Available = TransitGatewayState' "available"

pattern TransitGatewayState_Deleted :: TransitGatewayState
pattern TransitGatewayState_Deleted = TransitGatewayState' "deleted"

pattern TransitGatewayState_Deleting :: TransitGatewayState
pattern TransitGatewayState_Deleting = TransitGatewayState' "deleting"

pattern TransitGatewayState_Modifying :: TransitGatewayState
pattern TransitGatewayState_Modifying = TransitGatewayState' "modifying"

pattern TransitGatewayState_Pending :: TransitGatewayState
pattern TransitGatewayState_Pending = TransitGatewayState' "pending"

{-# COMPLETE
  TransitGatewayState_Available,
  TransitGatewayState_Deleted,
  TransitGatewayState_Deleting,
  TransitGatewayState_Modifying,
  TransitGatewayState_Pending,
  TransitGatewayState'
  #-}
