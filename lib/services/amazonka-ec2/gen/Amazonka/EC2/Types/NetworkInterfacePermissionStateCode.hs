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
-- Module      : Amazonka.EC2.Types.NetworkInterfacePermissionStateCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfacePermissionStateCode
  ( NetworkInterfacePermissionStateCode
      ( ..,
        NetworkInterfacePermissionStateCode_Granted,
        NetworkInterfacePermissionStateCode_Pending,
        NetworkInterfacePermissionStateCode_Revoked,
        NetworkInterfacePermissionStateCode_Revoking
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype NetworkInterfacePermissionStateCode = NetworkInterfacePermissionStateCode'
  { fromNetworkInterfacePermissionStateCode ::
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

pattern NetworkInterfacePermissionStateCode_Granted :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCode_Granted = NetworkInterfacePermissionStateCode' "granted"

pattern NetworkInterfacePermissionStateCode_Pending :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCode_Pending = NetworkInterfacePermissionStateCode' "pending"

pattern NetworkInterfacePermissionStateCode_Revoked :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCode_Revoked = NetworkInterfacePermissionStateCode' "revoked"

pattern NetworkInterfacePermissionStateCode_Revoking :: NetworkInterfacePermissionStateCode
pattern NetworkInterfacePermissionStateCode_Revoking = NetworkInterfacePermissionStateCode' "revoking"

{-# COMPLETE
  NetworkInterfacePermissionStateCode_Granted,
  NetworkInterfacePermissionStateCode_Pending,
  NetworkInterfacePermissionStateCode_Revoked,
  NetworkInterfacePermissionStateCode_Revoking,
  NetworkInterfacePermissionStateCode'
  #-}
