{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentState
  ( DirectConnectGatewayAttachmentState
      ( DirectConnectGatewayAttachmentState',
        Attaching,
        Attached,
        Detaching,
        Detached
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Attaching :: DirectConnectGatewayAttachmentState
pattern Attaching = DirectConnectGatewayAttachmentState' "attaching"

pattern Attached :: DirectConnectGatewayAttachmentState
pattern Attached = DirectConnectGatewayAttachmentState' "attached"

pattern Detaching :: DirectConnectGatewayAttachmentState
pattern Detaching = DirectConnectGatewayAttachmentState' "detaching"

pattern Detached :: DirectConnectGatewayAttachmentState
pattern Detached = DirectConnectGatewayAttachmentState' "detached"

{-# COMPLETE
  Attaching,
  Attached,
  Detaching,
  Detached,
  DirectConnectGatewayAttachmentState'
  #-}
