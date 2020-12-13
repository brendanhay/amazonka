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
        Creating,
        Created,
        Activating,
        Activated,
        Deactivating,
        Deactivated,
        Deleting,
        Updating
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

pattern Creating :: TriggerState
pattern Creating = TriggerState' "CREATING"

pattern Created :: TriggerState
pattern Created = TriggerState' "CREATED"

pattern Activating :: TriggerState
pattern Activating = TriggerState' "ACTIVATING"

pattern Activated :: TriggerState
pattern Activated = TriggerState' "ACTIVATED"

pattern Deactivating :: TriggerState
pattern Deactivating = TriggerState' "DEACTIVATING"

pattern Deactivated :: TriggerState
pattern Deactivated = TriggerState' "DEACTIVATED"

pattern Deleting :: TriggerState
pattern Deleting = TriggerState' "DELETING"

pattern Updating :: TriggerState
pattern Updating = TriggerState' "UPDATING"

{-# COMPLETE
  Creating,
  Created,
  Activating,
  Activated,
  Deactivating,
  Deactivated,
  Deleting,
  Updating,
  TriggerState'
  #-}
