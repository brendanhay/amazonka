{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationStatus
  ( ReplicationStatus
      ( ReplicationStatus',
        Completed,
        Failed,
        Pending,
        Replica
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype ReplicationStatus = ReplicationStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Completed :: ReplicationStatus
pattern Completed = ReplicationStatus' "COMPLETED"

pattern Failed :: ReplicationStatus
pattern Failed = ReplicationStatus' "FAILED"

pattern Pending :: ReplicationStatus
pattern Pending = ReplicationStatus' "PENDING"

pattern Replica :: ReplicationStatus
pattern Replica = ReplicationStatus' "REPLICA"

{-# COMPLETE
  Completed,
  Failed,
  Pending,
  Replica,
  ReplicationStatus'
  #-}
