{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceAvailability
  ( DeviceAvailability
      ( DeviceAvailability',
        Available,
        Busy,
        HighlyAvailable,
        TemporaryNotAvailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeviceAvailability = DeviceAvailability' Lude.Text
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

pattern Available :: DeviceAvailability
pattern Available = DeviceAvailability' "AVAILABLE"

pattern Busy :: DeviceAvailability
pattern Busy = DeviceAvailability' "BUSY"

pattern HighlyAvailable :: DeviceAvailability
pattern HighlyAvailable = DeviceAvailability' "HIGHLY_AVAILABLE"

pattern TemporaryNotAvailable :: DeviceAvailability
pattern TemporaryNotAvailable = DeviceAvailability' "TEMPORARY_NOT_AVAILABLE"

{-# COMPLETE
  Available,
  Busy,
  HighlyAvailable,
  TemporaryNotAvailable,
  DeviceAvailability'
  #-}
