-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DeviceUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DeviceUpdateStatus
  ( DeviceUpdateStatus
      ( DeviceUpdateStatus',
        NotUpToDate,
        UpToDate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The status of software on the input device.
newtype DeviceUpdateStatus = DeviceUpdateStatus' Lude.Text
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

pattern NotUpToDate :: DeviceUpdateStatus
pattern NotUpToDate = DeviceUpdateStatus' "NOT_UP_TO_DATE"

pattern UpToDate :: DeviceUpdateStatus
pattern UpToDate = DeviceUpdateStatus' "UP_TO_DATE"

{-# COMPLETE
  NotUpToDate,
  UpToDate,
  DeviceUpdateStatus'
  #-}
