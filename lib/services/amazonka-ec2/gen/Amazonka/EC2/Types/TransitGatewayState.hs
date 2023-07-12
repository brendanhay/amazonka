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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayState = TransitGatewayState'
  { fromTransitGatewayState ::
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
