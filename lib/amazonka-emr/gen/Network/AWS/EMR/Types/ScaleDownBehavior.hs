-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScaleDownBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScaleDownBehavior
  ( ScaleDownBehavior
      ( ScaleDownBehavior',
        TerminateAtInstanceHour,
        TerminateAtTaskCompletion
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScaleDownBehavior = ScaleDownBehavior' Lude.Text
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

pattern TerminateAtInstanceHour :: ScaleDownBehavior
pattern TerminateAtInstanceHour = ScaleDownBehavior' "TERMINATE_AT_INSTANCE_HOUR"

pattern TerminateAtTaskCompletion :: ScaleDownBehavior
pattern TerminateAtTaskCompletion = ScaleDownBehavior' "TERMINATE_AT_TASK_COMPLETION"

{-# COMPLETE
  TerminateAtInstanceHour,
  TerminateAtTaskCompletion,
  ScaleDownBehavior'
  #-}
