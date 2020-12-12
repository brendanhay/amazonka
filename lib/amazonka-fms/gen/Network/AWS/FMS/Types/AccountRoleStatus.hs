{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AccountRoleStatus
  ( AccountRoleStatus
      ( AccountRoleStatus',
        Creating,
        Deleted,
        Deleting,
        PendingDeletion,
        Ready
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AccountRoleStatus = AccountRoleStatus' Lude.Text
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

pattern Creating :: AccountRoleStatus
pattern Creating = AccountRoleStatus' "CREATING"

pattern Deleted :: AccountRoleStatus
pattern Deleted = AccountRoleStatus' "DELETED"

pattern Deleting :: AccountRoleStatus
pattern Deleting = AccountRoleStatus' "DELETING"

pattern PendingDeletion :: AccountRoleStatus
pattern PendingDeletion = AccountRoleStatus' "PENDING_DELETION"

pattern Ready :: AccountRoleStatus
pattern Ready = AccountRoleStatus' "READY"

{-# COMPLETE
  Creating,
  Deleted,
  Deleting,
  PendingDeletion,
  Ready,
  AccountRoleStatus'
  #-}
