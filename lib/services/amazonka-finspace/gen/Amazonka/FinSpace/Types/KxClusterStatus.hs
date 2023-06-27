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
-- Module      : Amazonka.FinSpace.Types.KxClusterStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxClusterStatus
  ( KxClusterStatus
      ( ..,
        KxClusterStatus_CREATE_FAILED,
        KxClusterStatus_CREATING,
        KxClusterStatus_DELETED,
        KxClusterStatus_DELETE_FAILED,
        KxClusterStatus_DELETING,
        KxClusterStatus_PENDING,
        KxClusterStatus_RUNNING,
        KxClusterStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KxClusterStatus = KxClusterStatus'
  { fromKxClusterStatus ::
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

pattern KxClusterStatus_CREATE_FAILED :: KxClusterStatus
pattern KxClusterStatus_CREATE_FAILED = KxClusterStatus' "CREATE_FAILED"

pattern KxClusterStatus_CREATING :: KxClusterStatus
pattern KxClusterStatus_CREATING = KxClusterStatus' "CREATING"

pattern KxClusterStatus_DELETED :: KxClusterStatus
pattern KxClusterStatus_DELETED = KxClusterStatus' "DELETED"

pattern KxClusterStatus_DELETE_FAILED :: KxClusterStatus
pattern KxClusterStatus_DELETE_FAILED = KxClusterStatus' "DELETE_FAILED"

pattern KxClusterStatus_DELETING :: KxClusterStatus
pattern KxClusterStatus_DELETING = KxClusterStatus' "DELETING"

pattern KxClusterStatus_PENDING :: KxClusterStatus
pattern KxClusterStatus_PENDING = KxClusterStatus' "PENDING"

pattern KxClusterStatus_RUNNING :: KxClusterStatus
pattern KxClusterStatus_RUNNING = KxClusterStatus' "RUNNING"

pattern KxClusterStatus_UPDATING :: KxClusterStatus
pattern KxClusterStatus_UPDATING = KxClusterStatus' "UPDATING"

{-# COMPLETE
  KxClusterStatus_CREATE_FAILED,
  KxClusterStatus_CREATING,
  KxClusterStatus_DELETED,
  KxClusterStatus_DELETE_FAILED,
  KxClusterStatus_DELETING,
  KxClusterStatus_PENDING,
  KxClusterStatus_RUNNING,
  KxClusterStatus_UPDATING,
  KxClusterStatus'
  #-}
