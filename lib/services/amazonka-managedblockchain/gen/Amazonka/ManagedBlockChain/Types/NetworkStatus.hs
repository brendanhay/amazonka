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
-- Module      : Amazonka.ManagedBlockChain.Types.NetworkStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NetworkStatus
  ( NetworkStatus
      ( ..,
        NetworkStatus_AVAILABLE,
        NetworkStatus_CREATE_FAILED,
        NetworkStatus_CREATING,
        NetworkStatus_DELETED,
        NetworkStatus_DELETING
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

pattern NetworkStatus_CREATE_FAILED :: NetworkStatus
pattern NetworkStatus_CREATE_FAILED = NetworkStatus' "CREATE_FAILED"

pattern NetworkStatus_CREATING :: NetworkStatus
pattern NetworkStatus_CREATING = NetworkStatus' "CREATING"

pattern NetworkStatus_DELETED :: NetworkStatus
pattern NetworkStatus_DELETED = NetworkStatus' "DELETED"

pattern NetworkStatus_DELETING :: NetworkStatus
pattern NetworkStatus_DELETING = NetworkStatus' "DELETING"

{-# COMPLETE
  NetworkStatus_AVAILABLE,
  NetworkStatus_CREATE_FAILED,
  NetworkStatus_CREATING,
  NetworkStatus_DELETED,
  NetworkStatus_DELETING,
  NetworkStatus'
  #-}
