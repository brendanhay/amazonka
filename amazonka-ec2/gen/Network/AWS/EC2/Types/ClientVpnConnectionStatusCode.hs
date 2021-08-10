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
-- Module      : Network.AWS.EC2.Types.ClientVpnConnectionStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnConnectionStatusCode
  ( ClientVpnConnectionStatusCode
      ( ..,
        ClientVpnConnectionStatusCode_Active,
        ClientVpnConnectionStatusCode_Failed_to_terminate,
        ClientVpnConnectionStatusCode_Terminated,
        ClientVpnConnectionStatusCode_Terminating
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ClientVpnConnectionStatusCode = ClientVpnConnectionStatusCode'
  { fromClientVpnConnectionStatusCode ::
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

pattern ClientVpnConnectionStatusCode_Active :: ClientVpnConnectionStatusCode
pattern ClientVpnConnectionStatusCode_Active = ClientVpnConnectionStatusCode' "active"

pattern ClientVpnConnectionStatusCode_Failed_to_terminate :: ClientVpnConnectionStatusCode
pattern ClientVpnConnectionStatusCode_Failed_to_terminate = ClientVpnConnectionStatusCode' "failed-to-terminate"

pattern ClientVpnConnectionStatusCode_Terminated :: ClientVpnConnectionStatusCode
pattern ClientVpnConnectionStatusCode_Terminated = ClientVpnConnectionStatusCode' "terminated"

pattern ClientVpnConnectionStatusCode_Terminating :: ClientVpnConnectionStatusCode
pattern ClientVpnConnectionStatusCode_Terminating = ClientVpnConnectionStatusCode' "terminating"

{-# COMPLETE
  ClientVpnConnectionStatusCode_Active,
  ClientVpnConnectionStatusCode_Failed_to_terminate,
  ClientVpnConnectionStatusCode_Terminated,
  ClientVpnConnectionStatusCode_Terminating,
  ClientVpnConnectionStatusCode'
  #-}
