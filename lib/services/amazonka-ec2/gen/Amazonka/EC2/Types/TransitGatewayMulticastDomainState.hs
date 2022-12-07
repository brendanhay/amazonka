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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDomainState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDomainState
  ( TransitGatewayMulticastDomainState
      ( ..,
        TransitGatewayMulticastDomainState_Available,
        TransitGatewayMulticastDomainState_Deleted,
        TransitGatewayMulticastDomainState_Deleting,
        TransitGatewayMulticastDomainState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayMulticastDomainState = TransitGatewayMulticastDomainState'
  { fromTransitGatewayMulticastDomainState ::
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

pattern TransitGatewayMulticastDomainState_Available :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Available = TransitGatewayMulticastDomainState' "available"

pattern TransitGatewayMulticastDomainState_Deleted :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Deleted = TransitGatewayMulticastDomainState' "deleted"

pattern TransitGatewayMulticastDomainState_Deleting :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Deleting = TransitGatewayMulticastDomainState' "deleting"

pattern TransitGatewayMulticastDomainState_Pending :: TransitGatewayMulticastDomainState
pattern TransitGatewayMulticastDomainState_Pending = TransitGatewayMulticastDomainState' "pending"

{-# COMPLETE
  TransitGatewayMulticastDomainState_Available,
  TransitGatewayMulticastDomainState_Deleted,
  TransitGatewayMulticastDomainState_Deleting,
  TransitGatewayMulticastDomainState_Pending,
  TransitGatewayMulticastDomainState'
  #-}
