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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
  ( DirectConnectGatewayAssociationProposalState
      ( ..,
        DirectConnectGatewayAssociationProposalState_Accepted,
        DirectConnectGatewayAssociationProposalState_Deleted,
        DirectConnectGatewayAssociationProposalState_Requested
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DirectConnectGatewayAssociationProposalState = DirectConnectGatewayAssociationProposalState'
  { fromDirectConnectGatewayAssociationProposalState ::
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

pattern DirectConnectGatewayAssociationProposalState_Accepted :: DirectConnectGatewayAssociationProposalState
pattern DirectConnectGatewayAssociationProposalState_Accepted = DirectConnectGatewayAssociationProposalState' "accepted"

pattern DirectConnectGatewayAssociationProposalState_Deleted :: DirectConnectGatewayAssociationProposalState
pattern DirectConnectGatewayAssociationProposalState_Deleted = DirectConnectGatewayAssociationProposalState' "deleted"

pattern DirectConnectGatewayAssociationProposalState_Requested :: DirectConnectGatewayAssociationProposalState
pattern DirectConnectGatewayAssociationProposalState_Requested = DirectConnectGatewayAssociationProposalState' "requested"

{-# COMPLETE
  DirectConnectGatewayAssociationProposalState_Accepted,
  DirectConnectGatewayAssociationProposalState_Deleted,
  DirectConnectGatewayAssociationProposalState_Requested,
  DirectConnectGatewayAssociationProposalState'
  #-}
