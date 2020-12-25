{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifeCycleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifeCycleState
  ( LifeCycleState
      ( LifeCycleState',
        LifeCycleStateCreating,
        LifeCycleStateAvailable,
        LifeCycleStateUpdating,
        LifeCycleStateDeleting,
        LifeCycleStateDeleted,
        fromLifeCycleState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LifeCycleState = LifeCycleState'
  { fromLifeCycleState ::
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

pattern LifeCycleStateCreating :: LifeCycleState
pattern LifeCycleStateCreating = LifeCycleState' "creating"

pattern LifeCycleStateAvailable :: LifeCycleState
pattern LifeCycleStateAvailable = LifeCycleState' "available"

pattern LifeCycleStateUpdating :: LifeCycleState
pattern LifeCycleStateUpdating = LifeCycleState' "updating"

pattern LifeCycleStateDeleting :: LifeCycleState
pattern LifeCycleStateDeleting = LifeCycleState' "deleting"

pattern LifeCycleStateDeleted :: LifeCycleState
pattern LifeCycleStateDeleted = LifeCycleState' "deleted"

{-# COMPLETE
  LifeCycleStateCreating,
  LifeCycleStateAvailable,
  LifeCycleStateUpdating,
  LifeCycleStateDeleting,
  LifeCycleStateDeleted,
  LifeCycleState'
  #-}
