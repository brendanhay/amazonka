-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.HSMStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.HSMStatus
  ( HSMStatus
      ( HSMStatus',
        HSDegraded,
        HSPending,
        HSRunning,
        HSSuspended,
        HSTerminated,
        HSTerminating,
        HSUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HSMStatus = HSMStatus' Lude.Text
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

pattern HSDegraded :: HSMStatus
pattern HSDegraded = HSMStatus' "DEGRADED"

pattern HSPending :: HSMStatus
pattern HSPending = HSMStatus' "PENDING"

pattern HSRunning :: HSMStatus
pattern HSRunning = HSMStatus' "RUNNING"

pattern HSSuspended :: HSMStatus
pattern HSSuspended = HSMStatus' "SUSPENDED"

pattern HSTerminated :: HSMStatus
pattern HSTerminated = HSMStatus' "TERMINATED"

pattern HSTerminating :: HSMStatus
pattern HSTerminating = HSMStatus' "TERMINATING"

pattern HSUpdating :: HSMStatus
pattern HSUpdating = HSMStatus' "UPDATING"

{-# COMPLETE
  HSDegraded,
  HSPending,
  HSRunning,
  HSSuspended,
  HSTerminated,
  HSTerminating,
  HSUpdating,
  HSMStatus'
  #-}
