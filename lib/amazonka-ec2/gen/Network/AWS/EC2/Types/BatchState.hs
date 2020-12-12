{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BatchState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BatchState
  ( BatchState
      ( BatchState',
        BSActive,
        BSCancelled,
        BSCancelledRunning,
        BSCancelledTerminating,
        BSFailed,
        BSModifying,
        BSSubmitted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BatchState = BatchState' Lude.Text
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

pattern BSActive :: BatchState
pattern BSActive = BatchState' "active"

pattern BSCancelled :: BatchState
pattern BSCancelled = BatchState' "cancelled"

pattern BSCancelledRunning :: BatchState
pattern BSCancelledRunning = BatchState' "cancelled_running"

pattern BSCancelledTerminating :: BatchState
pattern BSCancelledTerminating = BatchState' "cancelled_terminating"

pattern BSFailed :: BatchState
pattern BSFailed = BatchState' "failed"

pattern BSModifying :: BatchState
pattern BSModifying = BatchState' "modifying"

pattern BSSubmitted :: BatchState
pattern BSSubmitted = BatchState' "submitted"

{-# COMPLETE
  BSActive,
  BSCancelled,
  BSCancelledRunning,
  BSCancelledTerminating,
  BSFailed,
  BSModifying,
  BSSubmitted,
  BatchState'
  #-}
