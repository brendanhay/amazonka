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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayPrefixListReferenceState = TransitGatewayPrefixListReferenceState'
  { fromTransitGatewayPrefixListReferenceState ::
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
