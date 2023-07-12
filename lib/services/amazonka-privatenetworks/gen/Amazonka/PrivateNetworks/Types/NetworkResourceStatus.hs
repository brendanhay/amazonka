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
-- Module      : Amazonka.PrivateNetworks.Types.NetworkResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.NetworkResourceStatus
  ( NetworkResourceStatus
      ( ..,
        NetworkResourceStatus_AVAILABLE,
        NetworkResourceStatus_DELETED,
        NetworkResourceStatus_DELETING,
        NetworkResourceStatus_PENDING,
        NetworkResourceStatus_PENDING_RETURN,
        NetworkResourceStatus_PROVISIONED,
        NetworkResourceStatus_PROVISIONING,
        NetworkResourceStatus_SHIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NetworkResourceStatus = NetworkResourceStatus'
  { fromNetworkResourceStatus ::
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

pattern NetworkResourceStatus_AVAILABLE :: NetworkResourceStatus
pattern NetworkResourceStatus_AVAILABLE = NetworkResourceStatus' "AVAILABLE"

pattern NetworkResourceStatus_DELETED :: NetworkResourceStatus
pattern NetworkResourceStatus_DELETED = NetworkResourceStatus' "DELETED"

pattern NetworkResourceStatus_DELETING :: NetworkResourceStatus
pattern NetworkResourceStatus_DELETING = NetworkResourceStatus' "DELETING"

pattern NetworkResourceStatus_PENDING :: NetworkResourceStatus
pattern NetworkResourceStatus_PENDING = NetworkResourceStatus' "PENDING"

pattern NetworkResourceStatus_PENDING_RETURN :: NetworkResourceStatus
pattern NetworkResourceStatus_PENDING_RETURN = NetworkResourceStatus' "PENDING_RETURN"

pattern NetworkResourceStatus_PROVISIONED :: NetworkResourceStatus
pattern NetworkResourceStatus_PROVISIONED = NetworkResourceStatus' "PROVISIONED"

pattern NetworkResourceStatus_PROVISIONING :: NetworkResourceStatus
pattern NetworkResourceStatus_PROVISIONING = NetworkResourceStatus' "PROVISIONING"

pattern NetworkResourceStatus_SHIPPED :: NetworkResourceStatus
pattern NetworkResourceStatus_SHIPPED = NetworkResourceStatus' "SHIPPED"

{-# COMPLETE
  NetworkResourceStatus_AVAILABLE,
  NetworkResourceStatus_DELETED,
  NetworkResourceStatus_DELETING,
  NetworkResourceStatus_PENDING,
  NetworkResourceStatus_PENDING_RETURN,
  NetworkResourceStatus_PROVISIONED,
  NetworkResourceStatus_PROVISIONING,
  NetworkResourceStatus_SHIPPED,
  NetworkResourceStatus'
  #-}
