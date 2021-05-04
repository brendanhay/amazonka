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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState
  ( TransitGatewayPrefixListReferenceState
      ( ..,
        TransitGatewayPrefixListReferenceState_Available,
        TransitGatewayPrefixListReferenceState_Deleting,
        TransitGatewayPrefixListReferenceState_Modifying,
        TransitGatewayPrefixListReferenceState_Pending
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TransitGatewayPrefixListReferenceState = TransitGatewayPrefixListReferenceState'
  { fromTransitGatewayPrefixListReferenceState ::
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
