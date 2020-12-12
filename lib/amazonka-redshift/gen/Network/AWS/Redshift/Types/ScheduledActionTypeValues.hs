{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionTypeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionTypeValues
  ( ScheduledActionTypeValues
      ( ScheduledActionTypeValues',
        PauseCluster,
        ResizeCluster,
        ResumeCluster
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

newtype ScheduledActionTypeValues = ScheduledActionTypeValues' Lude.Text
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

pattern PauseCluster :: ScheduledActionTypeValues
pattern PauseCluster = ScheduledActionTypeValues' "PauseCluster"

pattern ResizeCluster :: ScheduledActionTypeValues
pattern ResizeCluster = ScheduledActionTypeValues' "ResizeCluster"

pattern ResumeCluster :: ScheduledActionTypeValues
pattern ResumeCluster = ScheduledActionTypeValues' "ResumeCluster"

{-# COMPLETE
  PauseCluster,
  ResizeCluster,
  ResumeCluster,
  ScheduledActionTypeValues'
  #-}
