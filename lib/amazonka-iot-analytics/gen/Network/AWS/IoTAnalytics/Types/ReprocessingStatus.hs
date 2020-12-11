-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingStatus
  ( ReprocessingStatus
      ( ReprocessingStatus',
        Cancelled,
        Failed,
        Running,
        Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReprocessingStatus = ReprocessingStatus' Lude.Text
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

pattern Cancelled :: ReprocessingStatus
pattern Cancelled = ReprocessingStatus' "CANCELLED"

pattern Failed :: ReprocessingStatus
pattern Failed = ReprocessingStatus' "FAILED"

pattern Running :: ReprocessingStatus
pattern Running = ReprocessingStatus' "RUNNING"

pattern Succeeded :: ReprocessingStatus
pattern Succeeded = ReprocessingStatus' "SUCCEEDED"

{-# COMPLETE
  Cancelled,
  Failed,
  Running,
  Succeeded,
  ReprocessingStatus'
  #-}
