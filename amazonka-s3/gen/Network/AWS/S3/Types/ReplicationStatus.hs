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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype ReplicationStatus = ReplicationStatus'
  { fromReplicationStatus ::
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
