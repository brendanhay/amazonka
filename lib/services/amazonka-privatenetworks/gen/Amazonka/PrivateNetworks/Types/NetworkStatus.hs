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
-- Module      : Amazonka.PrivateNetworks.Types.NetworkStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.NetworkStatus
  ( NetworkStatus
      ( ..,
        NetworkStatus_AVAILABLE,
        NetworkStatus_CREATED,
        NetworkStatus_DELETED,
        NetworkStatus_DEPROVISIONING,
        NetworkStatus_PROVISIONING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NetworkStatus = NetworkStatus'
  { fromNetworkStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern NetworkStatus_AVAILABLE :: NetworkStatus
pattern NetworkStatus_AVAILABLE = NetworkStatus' "AVAILABLE"

pattern NetworkStatus_CREATED :: NetworkStatus
pattern NetworkStatus_CREATED = NetworkStatus' "CREATED"

pattern NetworkStatus_DELETED :: NetworkStatus
pattern NetworkStatus_DELETED = NetworkStatus' "DELETED"

pattern NetworkStatus_DEPROVISIONING :: NetworkStatus
pattern NetworkStatus_DEPROVISIONING = NetworkStatus' "DEPROVISIONING"

pattern NetworkStatus_PROVISIONING :: NetworkStatus
pattern NetworkStatus_PROVISIONING = NetworkStatus' "PROVISIONING"

{-# COMPLETE
  NetworkStatus_AVAILABLE,
  NetworkStatus_CREATED,
  NetworkStatus_DELETED,
  NetworkStatus_DEPROVISIONING,
  NetworkStatus_PROVISIONING,
  NetworkStatus'
  #-}
