{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
  ( EnvironmentLifecycleStatus
      ( EnvironmentLifecycleStatus',
        EnvironmentLifecycleStatusCreating,
        EnvironmentLifecycleStatusCreated,
        EnvironmentLifecycleStatusCreateFailed,
        EnvironmentLifecycleStatusDeleting,
        EnvironmentLifecycleStatusDeleteFailed,
        fromEnvironmentLifecycleStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EnvironmentLifecycleStatus = EnvironmentLifecycleStatus'
  { fromEnvironmentLifecycleStatus ::
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

pattern EnvironmentLifecycleStatusCreating :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatusCreating = EnvironmentLifecycleStatus' "CREATING"

pattern EnvironmentLifecycleStatusCreated :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatusCreated = EnvironmentLifecycleStatus' "CREATED"

pattern EnvironmentLifecycleStatusCreateFailed :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatusCreateFailed = EnvironmentLifecycleStatus' "CREATE_FAILED"

pattern EnvironmentLifecycleStatusDeleting :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatusDeleting = EnvironmentLifecycleStatus' "DELETING"

pattern EnvironmentLifecycleStatusDeleteFailed :: EnvironmentLifecycleStatus
pattern EnvironmentLifecycleStatusDeleteFailed = EnvironmentLifecycleStatus' "DELETE_FAILED"

{-# COMPLETE
  EnvironmentLifecycleStatusCreating,
  EnvironmentLifecycleStatusCreated,
  EnvironmentLifecycleStatusCreateFailed,
  EnvironmentLifecycleStatusDeleting,
  EnvironmentLifecycleStatusDeleteFailed,
  EnvironmentLifecycleStatus'
  #-}
