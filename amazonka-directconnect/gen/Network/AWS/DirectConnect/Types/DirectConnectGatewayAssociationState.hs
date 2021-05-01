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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
  ( DirectConnectGatewayAssociationState
      ( ..,
        DirectConnectGatewayAssociationState_Associated,
        DirectConnectGatewayAssociationState_Associating,
        DirectConnectGatewayAssociationState_Disassociated,
        DirectConnectGatewayAssociationState_Disassociating,
        DirectConnectGatewayAssociationState_Updating
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState'
  { fromDirectConnectGatewayAssociationState ::
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

pattern DirectConnectGatewayAssociationState_Associated :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationState_Associated = DirectConnectGatewayAssociationState' "associated"

pattern DirectConnectGatewayAssociationState_Associating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationState_Associating = DirectConnectGatewayAssociationState' "associating"

pattern DirectConnectGatewayAssociationState_Disassociated :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationState_Disassociated = DirectConnectGatewayAssociationState' "disassociated"

pattern DirectConnectGatewayAssociationState_Disassociating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationState_Disassociating = DirectConnectGatewayAssociationState' "disassociating"

pattern DirectConnectGatewayAssociationState_Updating :: DirectConnectGatewayAssociationState
pattern DirectConnectGatewayAssociationState_Updating = DirectConnectGatewayAssociationState' "updating"

{-# COMPLETE
  DirectConnectGatewayAssociationState_Associated,
  DirectConnectGatewayAssociationState_Associating,
  DirectConnectGatewayAssociationState_Disassociated,
  DirectConnectGatewayAssociationState_Disassociating,
  DirectConnectGatewayAssociationState_Updating,
  DirectConnectGatewayAssociationState'
  #-}
