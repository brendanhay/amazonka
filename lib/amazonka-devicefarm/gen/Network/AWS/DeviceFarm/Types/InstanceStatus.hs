{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        InstanceStatusInUse,
        InstanceStatusPreparing,
        InstanceStatusAvailable,
        InstanceStatusNotAvailable,
        fromInstanceStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatusInUse :: InstanceStatus
pattern InstanceStatusInUse = InstanceStatus' "IN_USE"

pattern InstanceStatusPreparing :: InstanceStatus
pattern InstanceStatusPreparing = InstanceStatus' "PREPARING"

pattern InstanceStatusAvailable :: InstanceStatus
pattern InstanceStatusAvailable = InstanceStatus' "AVAILABLE"

pattern InstanceStatusNotAvailable :: InstanceStatus
pattern InstanceStatusNotAvailable = InstanceStatus' "NOT_AVAILABLE"

{-# COMPLETE
  InstanceStatusInUse,
  InstanceStatusPreparing,
  InstanceStatusAvailable,
  InstanceStatusNotAvailable,
  InstanceStatus'
  #-}
