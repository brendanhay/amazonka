-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatus
  ( CloseStatus
      ( CloseStatus',
        Canceled,
        Completed,
        ContinuedAsNew,
        Failed,
        Terminated,
        TimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CloseStatus = CloseStatus' Lude.Text
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

pattern Canceled :: CloseStatus
pattern Canceled = CloseStatus' "CANCELED"

pattern Completed :: CloseStatus
pattern Completed = CloseStatus' "COMPLETED"

pattern ContinuedAsNew :: CloseStatus
pattern ContinuedAsNew = CloseStatus' "CONTINUED_AS_NEW"

pattern Failed :: CloseStatus
pattern Failed = CloseStatus' "FAILED"

pattern Terminated :: CloseStatus
pattern Terminated = CloseStatus' "TERMINATED"

pattern TimedOut :: CloseStatus
pattern TimedOut = CloseStatus' "TIMED_OUT"

{-# COMPLETE
  Canceled,
  Completed,
  ContinuedAsNew,
  Failed,
  Terminated,
  TimedOut,
  CloseStatus'
  #-}
