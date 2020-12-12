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
        Available,
        Creating,
        Deleted,
        Deleting,
        Updating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LifeCycleState = LifeCycleState' Lude.Text
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

pattern Available :: LifeCycleState
pattern Available = LifeCycleState' "available"

pattern Creating :: LifeCycleState
pattern Creating = LifeCycleState' "creating"

pattern Deleted :: LifeCycleState
pattern Deleted = LifeCycleState' "deleted"

pattern Deleting :: LifeCycleState
pattern Deleting = LifeCycleState' "deleting"

pattern Updating :: LifeCycleState
pattern Updating = LifeCycleState' "updating"

{-# COMPLETE
  Available,
  Creating,
  Deleted,
  Deleting,
  Updating,
  LifeCycleState'
  #-}
