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

import qualified Network.AWS.Core as Core

newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState'
  { fromDirectConnectGatewayAssociationState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
