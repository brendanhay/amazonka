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
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpointStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnEndpointStatusCode
  ( ClientVpnEndpointStatusCode
      ( ..,
        ClientVpnEndpointStatusCode_Available,
        ClientVpnEndpointStatusCode_Deleted,
        ClientVpnEndpointStatusCode_Deleting,
        ClientVpnEndpointStatusCode_Pending_associate
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ClientVpnEndpointStatusCode = ClientVpnEndpointStatusCode'
  { fromClientVpnEndpointStatusCode ::
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
