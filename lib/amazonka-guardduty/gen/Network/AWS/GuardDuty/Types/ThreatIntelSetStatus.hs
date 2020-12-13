{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
  ( ThreatIntelSetStatus
      ( ThreatIntelSetStatus',
        Inactive,
        Activating,
        Active,
        Deactivating,
        Error,
        DeletePending,
        Deleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ThreatIntelSetStatus = ThreatIntelSetStatus' Lude.Text
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

pattern Inactive :: ThreatIntelSetStatus
pattern Inactive = ThreatIntelSetStatus' "INACTIVE"

pattern Activating :: ThreatIntelSetStatus
pattern Activating = ThreatIntelSetStatus' "ACTIVATING"

pattern Active :: ThreatIntelSetStatus
pattern Active = ThreatIntelSetStatus' "ACTIVE"

pattern Deactivating :: ThreatIntelSetStatus
pattern Deactivating = ThreatIntelSetStatus' "DEACTIVATING"

pattern Error :: ThreatIntelSetStatus
pattern Error = ThreatIntelSetStatus' "ERROR"

pattern DeletePending :: ThreatIntelSetStatus
pattern DeletePending = ThreatIntelSetStatus' "DELETE_PENDING"

pattern Deleted :: ThreatIntelSetStatus
pattern Deleted = ThreatIntelSetStatus' "DELETED"

{-# COMPLETE
  Inactive,
  Activating,
  Active,
  Deactivating,
  Error,
  DeletePending,
  Deleted,
  ThreatIntelSetStatus'
  #-}
