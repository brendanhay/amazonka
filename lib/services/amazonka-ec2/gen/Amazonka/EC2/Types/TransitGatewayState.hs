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
-- Module      : Amazonka.EC2.Types.TransitGatewayState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayState
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

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayState = TransitGatewayState'
  { fromTransitGatewayState ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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
