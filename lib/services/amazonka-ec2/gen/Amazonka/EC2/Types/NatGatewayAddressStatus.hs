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
-- Module      : Amazonka.EC2.Types.NatGatewayAddressStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NatGatewayAddressStatus
  ( NatGatewayAddressStatus
      ( ..,
        NatGatewayAddressStatus_Assigning,
        NatGatewayAddressStatus_Associating,
        NatGatewayAddressStatus_Disassociating,
        NatGatewayAddressStatus_Failed,
        NatGatewayAddressStatus_Succeeded,
        NatGatewayAddressStatus_Unassigning
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype NatGatewayAddressStatus = NatGatewayAddressStatus'
  { fromNatGatewayAddressStatus ::
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

pattern NatGatewayAddressStatus_Assigning :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Assigning = NatGatewayAddressStatus' "assigning"

pattern NatGatewayAddressStatus_Associating :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Associating = NatGatewayAddressStatus' "associating"

pattern NatGatewayAddressStatus_Disassociating :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Disassociating = NatGatewayAddressStatus' "disassociating"

pattern NatGatewayAddressStatus_Failed :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Failed = NatGatewayAddressStatus' "failed"

pattern NatGatewayAddressStatus_Succeeded :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Succeeded = NatGatewayAddressStatus' "succeeded"

pattern NatGatewayAddressStatus_Unassigning :: NatGatewayAddressStatus
pattern NatGatewayAddressStatus_Unassigning = NatGatewayAddressStatus' "unassigning"

{-# COMPLETE
  NatGatewayAddressStatus_Assigning,
  NatGatewayAddressStatus_Associating,
  NatGatewayAddressStatus_Disassociating,
  NatGatewayAddressStatus_Failed,
  NatGatewayAddressStatus_Succeeded,
  NatGatewayAddressStatus_Unassigning,
  NatGatewayAddressStatus'
  #-}
