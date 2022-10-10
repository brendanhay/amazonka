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
-- Module      : Amazonka.EC2.Types.ClientVpnEndpointStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnEndpointStatusCode
  ( ClientVpnEndpointStatusCode
      ( ..,
        ClientVpnEndpointStatusCode_Available,
        ClientVpnEndpointStatusCode_Deleted,
        ClientVpnEndpointStatusCode_Deleting,
        ClientVpnEndpointStatusCode_Pending_associate
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ClientVpnEndpointStatusCode = ClientVpnEndpointStatusCode'
  { fromClientVpnEndpointStatusCode ::
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

pattern ClientVpnEndpointStatusCode_Available :: ClientVpnEndpointStatusCode
pattern ClientVpnEndpointStatusCode_Available = ClientVpnEndpointStatusCode' "available"

pattern ClientVpnEndpointStatusCode_Deleted :: ClientVpnEndpointStatusCode
pattern ClientVpnEndpointStatusCode_Deleted = ClientVpnEndpointStatusCode' "deleted"

pattern ClientVpnEndpointStatusCode_Deleting :: ClientVpnEndpointStatusCode
pattern ClientVpnEndpointStatusCode_Deleting = ClientVpnEndpointStatusCode' "deleting"

pattern ClientVpnEndpointStatusCode_Pending_associate :: ClientVpnEndpointStatusCode
pattern ClientVpnEndpointStatusCode_Pending_associate = ClientVpnEndpointStatusCode' "pending-associate"

{-# COMPLETE
  ClientVpnEndpointStatusCode_Available,
  ClientVpnEndpointStatusCode_Deleted,
  ClientVpnEndpointStatusCode_Deleting,
  ClientVpnEndpointStatusCode_Pending_associate,
  ClientVpnEndpointStatusCode'
  #-}
