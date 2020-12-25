{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerState
  ( TriggerState
      ( TriggerState',
        TriggerStateCreating,
        TriggerStateCreated,
        TriggerStateActivating,
        TriggerStateActivated,
        TriggerStateDeactivating,
        TriggerStateDeactivated,
        TriggerStateDeleting,
        TriggerStateUpdating,
        fromTriggerState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TriggerState = TriggerState' {fromTriggerState :: Core.Text}
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

pattern TriggerStateCreating :: TriggerState
pattern TriggerStateCreating = TriggerState' "CREATING"

pattern TriggerStateCreated :: TriggerState
pattern TriggerStateCreated = TriggerState' "CREATED"

pattern TriggerStateActivating :: TriggerState
pattern TriggerStateActivating = TriggerState' "ACTIVATING"

pattern TriggerStateActivated :: TriggerState
pattern TriggerStateActivated = TriggerState' "ACTIVATED"

pattern TriggerStateDeactivating :: TriggerState
pattern TriggerStateDeactivating = TriggerState' "DEACTIVATING"

pattern TriggerStateDeactivated :: TriggerState
pattern TriggerStateDeactivated = TriggerState' "DEACTIVATED"

pattern TriggerStateDeleting :: TriggerState
pattern TriggerStateDeleting = TriggerState' "DELETING"

pattern TriggerStateUpdating :: TriggerState
pattern TriggerStateUpdating = TriggerState' "UPDATING"

{-# COMPLETE
  TriggerStateCreating,
  TriggerStateCreated,
  TriggerStateActivating,
  TriggerStateActivated,
  TriggerStateDeactivating,
  TriggerStateDeactivated,
  TriggerStateDeleting,
  TriggerStateUpdating,
  TriggerState'
  #-}
