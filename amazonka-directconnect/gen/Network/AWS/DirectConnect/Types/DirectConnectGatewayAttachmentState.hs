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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
  ( DirectConnectGatewayAttachmentState
      ( ..,
        DirectConnectGatewayAttachmentState_Attached,
        DirectConnectGatewayAttachmentState_Attaching,
        DirectConnectGatewayAttachmentState_Detached,
        DirectConnectGatewayAttachmentState_Detaching
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState'
  { fromDirectConnectGatewayAttachmentState ::
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
