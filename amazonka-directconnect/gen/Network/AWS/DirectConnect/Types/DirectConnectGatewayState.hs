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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayState
  ( DirectConnectGatewayState
      ( ..,
        DirectConnectGatewayState_Available,
        DirectConnectGatewayState_Deleted,
        DirectConnectGatewayState_Deleting,
        DirectConnectGatewayState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DirectConnectGatewayState = DirectConnectGatewayState'
  { fromDirectConnectGatewayState ::
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

pattern DirectConnectGatewayState_Available :: DirectConnectGatewayState
pattern DirectConnectGatewayState_Available = DirectConnectGatewayState' "available"

pattern DirectConnectGatewayState_Deleted :: DirectConnectGatewayState
pattern DirectConnectGatewayState_Deleted = DirectConnectGatewayState' "deleted"

pattern DirectConnectGatewayState_Deleting :: DirectConnectGatewayState
pattern DirectConnectGatewayState_Deleting = DirectConnectGatewayState' "deleting"

pattern DirectConnectGatewayState_Pending :: DirectConnectGatewayState
pattern DirectConnectGatewayState_Pending = DirectConnectGatewayState' "pending"

{-# COMPLETE
  DirectConnectGatewayState_Available,
  DirectConnectGatewayState_Deleted,
  DirectConnectGatewayState_Deleting,
  DirectConnectGatewayState_Pending,
  DirectConnectGatewayState'
  #-}
