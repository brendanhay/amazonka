{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAttachmentType
  ( DirectConnectGatewayAttachmentType
      ( DirectConnectGatewayAttachmentType',
        DirectConnectGatewayAttachmentTypeTransitVirtualInterface,
        DirectConnectGatewayAttachmentTypePrivateVirtualInterface,
        fromDirectConnectGatewayAttachmentType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DirectConnectGatewayAttachmentType = DirectConnectGatewayAttachmentType'
  { fromDirectConnectGatewayAttachmentType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DirectConnectGatewayAttachmentTypeTransitVirtualInterface :: DirectConnectGatewayAttachmentType
pattern DirectConnectGatewayAttachmentTypeTransitVirtualInterface = DirectConnectGatewayAttachmentType' "TransitVirtualInterface"

pattern DirectConnectGatewayAttachmentTypePrivateVirtualInterface :: DirectConnectGatewayAttachmentType
pattern DirectConnectGatewayAttachmentTypePrivateVirtualInterface = DirectConnectGatewayAttachmentType' "PrivateVirtualInterface"

{-# COMPLETE
  DirectConnectGatewayAttachmentTypeTransitVirtualInterface,
  DirectConnectGatewayAttachmentTypePrivateVirtualInterface,
  DirectConnectGatewayAttachmentType'
  #-}
