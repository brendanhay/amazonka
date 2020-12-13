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
        Creating,
        Created,
        CreateFailed,
        Deleting,
        DeleteFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentLifecycleStatus = EnvironmentLifecycleStatus' Lude.Text
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

pattern Creating :: EnvironmentLifecycleStatus
pattern Creating = EnvironmentLifecycleStatus' "CREATING"

pattern Created :: EnvironmentLifecycleStatus
pattern Created = EnvironmentLifecycleStatus' "CREATED"

pattern CreateFailed :: EnvironmentLifecycleStatus
pattern CreateFailed = EnvironmentLifecycleStatus' "CREATE_FAILED"

pattern Deleting :: EnvironmentLifecycleStatus
pattern Deleting = EnvironmentLifecycleStatus' "DELETING"

pattern DeleteFailed :: EnvironmentLifecycleStatus
pattern DeleteFailed = EnvironmentLifecycleStatus' "DELETE_FAILED"

{-# COMPLETE
  Creating,
  Created,
  CreateFailed,
  Deleting,
  DeleteFailed,
  EnvironmentLifecycleStatus'
  #-}
