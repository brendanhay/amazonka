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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationProposalState
  ( DirectConnectGatewayAssociationProposalState
      ( ..,
        DirectConnectGatewayAssociationProposalState_Accepted,
        DirectConnectGatewayAssociationProposalState_Deleted,
        DirectConnectGatewayAssociationProposalState_Requested
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DirectConnectGatewayAssociationProposalState = DirectConnectGatewayAssociationProposalState'
  { fromDirectConnectGatewayAssociationProposalState ::
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
