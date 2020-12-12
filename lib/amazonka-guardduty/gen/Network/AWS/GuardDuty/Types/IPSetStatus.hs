{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IPSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IPSetStatus
  ( IPSetStatus
      ( IPSetStatus',
        ISSActivating,
        ISSActive,
        ISSDeactivating,
        ISSDeletePending,
        ISSDeleted,
        ISSError,
        ISSInactive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IPSetStatus = IPSetStatus' Lude.Text
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

pattern ISSActivating :: IPSetStatus
pattern ISSActivating = IPSetStatus' "ACTIVATING"

pattern ISSActive :: IPSetStatus
pattern ISSActive = IPSetStatus' "ACTIVE"

pattern ISSDeactivating :: IPSetStatus
pattern ISSDeactivating = IPSetStatus' "DEACTIVATING"

pattern ISSDeletePending :: IPSetStatus
pattern ISSDeletePending = IPSetStatus' "DELETE_PENDING"

pattern ISSDeleted :: IPSetStatus
pattern ISSDeleted = IPSetStatus' "DELETED"

pattern ISSError :: IPSetStatus
pattern ISSError = IPSetStatus' "ERROR"

pattern ISSInactive :: IPSetStatus
pattern ISSInactive = IPSetStatus' "INACTIVE"

{-# COMPLETE
  ISSActivating,
  ISSActive,
  ISSDeactivating,
  ISSDeletePending,
  ISSDeleted,
  ISSError,
  ISSInactive,
  IPSetStatus'
  #-}
