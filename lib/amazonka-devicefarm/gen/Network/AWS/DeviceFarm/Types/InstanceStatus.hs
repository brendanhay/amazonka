-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceStatus
  ( InstanceStatus
      ( InstanceStatus',
        ISAvailable,
        ISInUse,
        ISNotAvailable,
        ISPreparing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceStatus = InstanceStatus' Lude.Text
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

pattern ISAvailable :: InstanceStatus
pattern ISAvailable = InstanceStatus' "AVAILABLE"

pattern ISInUse :: InstanceStatus
pattern ISInUse = InstanceStatus' "IN_USE"

pattern ISNotAvailable :: InstanceStatus
pattern ISNotAvailable = InstanceStatus' "NOT_AVAILABLE"

pattern ISPreparing :: InstanceStatus
pattern ISPreparing = InstanceStatus' "PREPARING"

{-# COMPLETE
  ISAvailable,
  ISInUse,
  ISNotAvailable,
  ISPreparing,
  InstanceStatus'
  #-}
