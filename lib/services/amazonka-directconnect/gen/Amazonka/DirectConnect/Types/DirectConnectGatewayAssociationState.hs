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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAssociationState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DirectConnectGatewayAssociationState = DirectConnectGatewayAssociationState'
  { fromDirectConnectGatewayAssociationState ::
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
