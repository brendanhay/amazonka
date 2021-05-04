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

import qualified Network.AWS.Prelude as Prelude

newtype DirectConnectGatewayAttachmentState = DirectConnectGatewayAttachmentState'
  { fromDirectConnectGatewayAttachmentState ::
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
