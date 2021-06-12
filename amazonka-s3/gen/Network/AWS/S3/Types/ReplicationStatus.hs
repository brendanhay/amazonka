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
-- Module      : Network.AWS.S3.Types.ReplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationStatus
  ( ReplicationStatus
      ( ..,
        ReplicationStatus_COMPLETED,
        ReplicationStatus_FAILED,
        ReplicationStatus_PENDING,
        ReplicationStatus_REPLICA
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype ReplicationStatus = ReplicationStatus'
  { fromReplicationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
