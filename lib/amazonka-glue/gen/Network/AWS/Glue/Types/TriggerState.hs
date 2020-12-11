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
        TSActivated,
        TSActivating,
        TSCreated,
        TSCreating,
        TSDeactivated,
        TSDeactivating,
        TSDeleting,
        TSUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TriggerState = TriggerState' Lude.Text
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

pattern TSActivated :: TriggerState
pattern TSActivated = TriggerState' "ACTIVATED"

pattern TSActivating :: TriggerState
pattern TSActivating = TriggerState' "ACTIVATING"

pattern TSCreated :: TriggerState
pattern TSCreated = TriggerState' "CREATED"

pattern TSCreating :: TriggerState
pattern TSCreating = TriggerState' "CREATING"

pattern TSDeactivated :: TriggerState
pattern TSDeactivated = TriggerState' "DEACTIVATED"

pattern TSDeactivating :: TriggerState
pattern TSDeactivating = TriggerState' "DEACTIVATING"

pattern TSDeleting :: TriggerState
pattern TSDeleting = TriggerState' "DELETING"

pattern TSUpdating :: TriggerState
pattern TSUpdating = TriggerState' "UPDATING"

{-# COMPLETE
  TSActivated,
  TSActivating,
  TSCreated,
  TSCreating,
  TSDeactivated,
  TSDeactivating,
  TSDeleting,
  TSUpdating,
  TriggerState'
  #-}
