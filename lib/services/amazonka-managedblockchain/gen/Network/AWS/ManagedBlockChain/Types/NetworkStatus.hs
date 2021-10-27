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
-- Module      : Network.AWS.ManagedBlockChain.Types.NetworkStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.NetworkStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype NetworkStatus = NetworkStatus'
  { fromNetworkStatus ::
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
