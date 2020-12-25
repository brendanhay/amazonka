{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceState
  ( ServiceState
      ( ServiceState',
        ServiceStatePending,
        ServiceStateAvailable,
        ServiceStateDeleting,
        ServiceStateDeleted,
        ServiceStateFailed,
        fromServiceState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ServiceState = ServiceState' {fromServiceState :: Core.Text}
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

pattern ServiceStatePending :: ServiceState
pattern ServiceStatePending = ServiceState' "Pending"

pattern ServiceStateAvailable :: ServiceState
pattern ServiceStateAvailable = ServiceState' "Available"

pattern ServiceStateDeleting :: ServiceState
pattern ServiceStateDeleting = ServiceState' "Deleting"

pattern ServiceStateDeleted :: ServiceState
pattern ServiceStateDeleted = ServiceState' "Deleted"

pattern ServiceStateFailed :: ServiceState
pattern ServiceStateFailed = ServiceState' "Failed"

{-# COMPLETE
  ServiceStatePending,
  ServiceStateAvailable,
  ServiceStateDeleting,
  ServiceStateDeleted,
  ServiceStateFailed,
  ServiceState'
  #-}
