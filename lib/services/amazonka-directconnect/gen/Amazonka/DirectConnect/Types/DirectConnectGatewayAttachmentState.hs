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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGatewayAttachmentState
  ( DirectConnectGatewayAttachmentState
      ( ..,
        DirectConnectGatewayAttachmentState_Attached,
        DirectConnectGatewayAttachmentState_Attaching,
        DirectConnectGatewayAttachmentState_Detached,
        DirectConnectGatewayAttachmentState_Detaching
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState'
  { fromDirectConnectGatewayAttachmentState ::
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

pattern DirectConnectGatewayAttachmentState_Attached :: DirectConnectGatewayAttachmentState
pattern DirectConnectGatewayAttachmentState_Attached = DirectConnectGatewayAttachmentState' "attached"

pattern DirectConnectGatewayAttachmentState_Attaching :: DirectConnectGatewayAttachmentState
pattern DirectConnectGatewayAttachmentState_Attaching = DirectConnectGatewayAttachmentState' "attaching"

pattern DirectConnectGatewayAttachmentState_Detached :: DirectConnectGatewayAttachmentState
pattern DirectConnectGatewayAttachmentState_Detached = DirectConnectGatewayAttachmentState' "detached"

pattern DirectConnectGatewayAttachmentState_Detaching :: DirectConnectGatewayAttachmentState
pattern DirectConnectGatewayAttachmentState_Detaching = DirectConnectGatewayAttachmentState' "detaching"

{-# COMPLETE
  DirectConnectGatewayAttachmentState_Attached,
  DirectConnectGatewayAttachmentState_Attaching,
  DirectConnectGatewayAttachmentState_Detached,
  DirectConnectGatewayAttachmentState_Detaching,
  DirectConnectGatewayAttachmentState'
  #-}
