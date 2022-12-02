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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementState
  ( TransitGatewayRouteTableAnnouncementState
      ( ..,
        TransitGatewayRouteTableAnnouncementState_Available,
        TransitGatewayRouteTableAnnouncementState_Deleted,
        TransitGatewayRouteTableAnnouncementState_Deleting,
        TransitGatewayRouteTableAnnouncementState_Failed,
        TransitGatewayRouteTableAnnouncementState_Failing,
        TransitGatewayRouteTableAnnouncementState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayRouteTableAnnouncementState = TransitGatewayRouteTableAnnouncementState'
  { fromTransitGatewayRouteTableAnnouncementState ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern TransitGatewayRouteTableAnnouncementState_Available :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Available = TransitGatewayRouteTableAnnouncementState' "available"

pattern TransitGatewayRouteTableAnnouncementState_Deleted :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Deleted = TransitGatewayRouteTableAnnouncementState' "deleted"

pattern TransitGatewayRouteTableAnnouncementState_Deleting :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Deleting = TransitGatewayRouteTableAnnouncementState' "deleting"

pattern TransitGatewayRouteTableAnnouncementState_Failed :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Failed = TransitGatewayRouteTableAnnouncementState' "failed"

pattern TransitGatewayRouteTableAnnouncementState_Failing :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Failing = TransitGatewayRouteTableAnnouncementState' "failing"

pattern TransitGatewayRouteTableAnnouncementState_Pending :: TransitGatewayRouteTableAnnouncementState
pattern TransitGatewayRouteTableAnnouncementState_Pending = TransitGatewayRouteTableAnnouncementState' "pending"

{-# COMPLETE
  TransitGatewayRouteTableAnnouncementState_Available,
  TransitGatewayRouteTableAnnouncementState_Deleted,
  TransitGatewayRouteTableAnnouncementState_Deleting,
  TransitGatewayRouteTableAnnouncementState_Failed,
  TransitGatewayRouteTableAnnouncementState_Failing,
  TransitGatewayRouteTableAnnouncementState_Pending,
  TransitGatewayRouteTableAnnouncementState'
  #-}
