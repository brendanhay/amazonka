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
-- Module      : Amazonka.EC2.Types.TransitGatewayPrefixListReferenceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPrefixListReferenceState
  ( TransitGatewayPrefixListReferenceState
      ( ..,
        TransitGatewayPrefixListReferenceState_Available,
        TransitGatewayPrefixListReferenceState_Deleting,
        TransitGatewayPrefixListReferenceState_Modifying,
        TransitGatewayPrefixListReferenceState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayPrefixListReferenceState = TransitGatewayPrefixListReferenceState'
  { fromTransitGatewayPrefixListReferenceState ::
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

pattern TransitGatewayPrefixListReferenceState_Available :: TransitGatewayPrefixListReferenceState
pattern TransitGatewayPrefixListReferenceState_Available = TransitGatewayPrefixListReferenceState' "available"

pattern TransitGatewayPrefixListReferenceState_Deleting :: TransitGatewayPrefixListReferenceState
pattern TransitGatewayPrefixListReferenceState_Deleting = TransitGatewayPrefixListReferenceState' "deleting"

pattern TransitGatewayPrefixListReferenceState_Modifying :: TransitGatewayPrefixListReferenceState
pattern TransitGatewayPrefixListReferenceState_Modifying = TransitGatewayPrefixListReferenceState' "modifying"

pattern TransitGatewayPrefixListReferenceState_Pending :: TransitGatewayPrefixListReferenceState
pattern TransitGatewayPrefixListReferenceState_Pending = TransitGatewayPrefixListReferenceState' "pending"

{-# COMPLETE
  TransitGatewayPrefixListReferenceState_Available,
  TransitGatewayPrefixListReferenceState_Deleting,
  TransitGatewayPrefixListReferenceState_Modifying,
  TransitGatewayPrefixListReferenceState_Pending,
  TransitGatewayPrefixListReferenceState'
  #-}
