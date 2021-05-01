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

import qualified Network.AWS.Prelude as Prelude

newtype DirectConnectGatewayState = DirectConnectGatewayState'
  { fromDirectConnectGatewayState ::
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
