{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndexStatus
  ( PartitionIndexStatus
      ( PartitionIndexStatus',
        PartitionIndexStatusCreating,
        PartitionIndexStatusActive,
        PartitionIndexStatusDeleting,
        PartitionIndexStatusFailed,
        fromPartitionIndexStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PartitionIndexStatus = PartitionIndexStatus'
  { fromPartitionIndexStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PartitionIndexStatusCreating :: PartitionIndexStatus
pattern PartitionIndexStatusCreating = PartitionIndexStatus' "CREATING"

pattern PartitionIndexStatusActive :: PartitionIndexStatus
pattern PartitionIndexStatusActive = PartitionIndexStatus' "ACTIVE"

pattern PartitionIndexStatusDeleting :: PartitionIndexStatus
pattern PartitionIndexStatusDeleting = PartitionIndexStatus' "DELETING"

pattern PartitionIndexStatusFailed :: PartitionIndexStatus
pattern PartitionIndexStatusFailed = PartitionIndexStatus' "FAILED"

{-# COMPLETE
  PartitionIndexStatusCreating,
  PartitionIndexStatusActive,
  PartitionIndexStatusDeleting,
  PartitionIndexStatusFailed,
  PartitionIndexStatus'
  #-}
