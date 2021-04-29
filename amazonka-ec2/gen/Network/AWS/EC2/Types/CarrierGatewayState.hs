{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGatewayState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGatewayState
  ( CarrierGatewayState
      ( ..,
        CarrierGatewayState_Available,
        CarrierGatewayState_Deleted,
        CarrierGatewayState_Deleting,
        CarrierGatewayState_Pending
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype CarrierGatewayState = CarrierGatewayState'
  { fromCarrierGatewayState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CarrierGatewayState_Available :: CarrierGatewayState
pattern CarrierGatewayState_Available = CarrierGatewayState' "available"

pattern CarrierGatewayState_Deleted :: CarrierGatewayState
pattern CarrierGatewayState_Deleted = CarrierGatewayState' "deleted"

pattern CarrierGatewayState_Deleting :: CarrierGatewayState
pattern CarrierGatewayState_Deleting = CarrierGatewayState' "deleting"

pattern CarrierGatewayState_Pending :: CarrierGatewayState
pattern CarrierGatewayState_Pending = CarrierGatewayState' "pending"

{-# COMPLETE
  CarrierGatewayState_Available,
  CarrierGatewayState_Deleted,
  CarrierGatewayState_Deleting,
  CarrierGatewayState_Pending,
  CarrierGatewayState'
  #-}
