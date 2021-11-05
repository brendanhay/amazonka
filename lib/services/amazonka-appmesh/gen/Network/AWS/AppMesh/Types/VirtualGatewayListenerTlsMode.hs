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
-- Module      : Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.VirtualGatewayListenerTlsMode
  ( VirtualGatewayListenerTlsMode
      ( ..,
        VirtualGatewayListenerTlsMode_DISABLED,
        VirtualGatewayListenerTlsMode_PERMISSIVE,
        VirtualGatewayListenerTlsMode_STRICT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype VirtualGatewayListenerTlsMode = VirtualGatewayListenerTlsMode'
  { fromVirtualGatewayListenerTlsMode ::
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

pattern VirtualGatewayListenerTlsMode_DISABLED :: VirtualGatewayListenerTlsMode
pattern VirtualGatewayListenerTlsMode_DISABLED = VirtualGatewayListenerTlsMode' "DISABLED"

pattern VirtualGatewayListenerTlsMode_PERMISSIVE :: VirtualGatewayListenerTlsMode
pattern VirtualGatewayListenerTlsMode_PERMISSIVE = VirtualGatewayListenerTlsMode' "PERMISSIVE"

pattern VirtualGatewayListenerTlsMode_STRICT :: VirtualGatewayListenerTlsMode
pattern VirtualGatewayListenerTlsMode_STRICT = VirtualGatewayListenerTlsMode' "STRICT"

{-# COMPLETE
  VirtualGatewayListenerTlsMode_DISABLED,
  VirtualGatewayListenerTlsMode_PERMISSIVE,
  VirtualGatewayListenerTlsMode_STRICT,
  VirtualGatewayListenerTlsMode'
  #-}
