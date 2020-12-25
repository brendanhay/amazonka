{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateStatus
  ( OTAUpdateStatus
      ( OTAUpdateStatus',
        OTAUpdateStatusCreatePending,
        OTAUpdateStatusCreateInProgress,
        OTAUpdateStatusCreateComplete,
        OTAUpdateStatusCreateFailed,
        fromOTAUpdateStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OTAUpdateStatus = OTAUpdateStatus'
  { fromOTAUpdateStatus ::
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

pattern OTAUpdateStatusCreatePending :: OTAUpdateStatus
pattern OTAUpdateStatusCreatePending = OTAUpdateStatus' "CREATE_PENDING"

pattern OTAUpdateStatusCreateInProgress :: OTAUpdateStatus
pattern OTAUpdateStatusCreateInProgress = OTAUpdateStatus' "CREATE_IN_PROGRESS"

pattern OTAUpdateStatusCreateComplete :: OTAUpdateStatus
pattern OTAUpdateStatusCreateComplete = OTAUpdateStatus' "CREATE_COMPLETE"

pattern OTAUpdateStatusCreateFailed :: OTAUpdateStatus
pattern OTAUpdateStatusCreateFailed = OTAUpdateStatus' "CREATE_FAILED"

{-# COMPLETE
  OTAUpdateStatusCreatePending,
  OTAUpdateStatusCreateInProgress,
  OTAUpdateStatusCreateComplete,
  OTAUpdateStatusCreateFailed,
  OTAUpdateStatus'
  #-}
