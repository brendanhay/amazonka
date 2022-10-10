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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayState
  ( DirectConnectGatewayState
      ( ..,
        DirectConnectGatewayState_Available,
        DirectConnectGatewayState_Deleted,
        DirectConnectGatewayState_Deleting,
        DirectConnectGatewayState_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DirectConnectGatewayState = DirectConnectGatewayState'
  { fromDirectConnectGatewayState ::
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
