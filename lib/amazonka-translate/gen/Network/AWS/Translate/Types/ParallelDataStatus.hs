{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataStatus
  ( ParallelDataStatus
      ( ParallelDataStatus',
        Creating,
        Updating,
        Active,
        Deleting,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ParallelDataStatus = ParallelDataStatus' Lude.Text
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

pattern Creating :: ParallelDataStatus
pattern Creating = ParallelDataStatus' "CREATING"

pattern Updating :: ParallelDataStatus
pattern Updating = ParallelDataStatus' "UPDATING"

pattern Active :: ParallelDataStatus
pattern Active = ParallelDataStatus' "ACTIVE"

pattern Deleting :: ParallelDataStatus
pattern Deleting = ParallelDataStatus' "DELETING"

pattern Failed :: ParallelDataStatus
pattern Failed = ParallelDataStatus' "FAILED"

{-# COMPLETE
  Creating,
  Updating,
  Active,
  Deleting,
  Failed,
  ParallelDataStatus'
  #-}
