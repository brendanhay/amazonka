{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupState
  ( BackupState
      ( BackupState',
        CreateInProgress,
        Deleted,
        PendingDeletion,
        Ready
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BackupState = BackupState' Lude.Text
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

pattern CreateInProgress :: BackupState
pattern CreateInProgress = BackupState' "CREATE_IN_PROGRESS"

pattern Deleted :: BackupState
pattern Deleted = BackupState' "DELETED"

pattern PendingDeletion :: BackupState
pattern PendingDeletion = BackupState' "PENDING_DELETION"

pattern Ready :: BackupState
pattern Ready = BackupState' "READY"

{-# COMPLETE
  CreateInProgress,
  Deleted,
  PendingDeletion,
  Ready,
  BackupState'
  #-}
