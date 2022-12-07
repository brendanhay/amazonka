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
-- Module      : Amazonka.S3.Types.ReplicationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ReplicationStatus
  ( ReplicationStatus
      ( ..,
        ReplicationStatus_COMPLETED,
        ReplicationStatus_FAILED,
        ReplicationStatus_PENDING,
        ReplicationStatus_REPLICA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ReplicationStatus = ReplicationStatus'
  { fromReplicationStatus ::
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

pattern ReplicationStatus_COMPLETED :: ReplicationStatus
pattern ReplicationStatus_COMPLETED = ReplicationStatus' "COMPLETED"

pattern ReplicationStatus_FAILED :: ReplicationStatus
pattern ReplicationStatus_FAILED = ReplicationStatus' "FAILED"

pattern ReplicationStatus_PENDING :: ReplicationStatus
pattern ReplicationStatus_PENDING = ReplicationStatus' "PENDING"

pattern ReplicationStatus_REPLICA :: ReplicationStatus
pattern ReplicationStatus_REPLICA = ReplicationStatus' "REPLICA"

{-# COMPLETE
  ReplicationStatus_COMPLETED,
  ReplicationStatus_FAILED,
  ReplicationStatus_PENDING,
  ReplicationStatus_REPLICA,
  ReplicationStatus'
  #-}
