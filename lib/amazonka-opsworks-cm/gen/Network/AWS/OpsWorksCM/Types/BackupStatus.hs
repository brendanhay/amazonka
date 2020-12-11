-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.BackupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.BackupStatus
  ( BackupStatus
      ( BackupStatus',
        BSDeleting,
        BSFailed,
        BSInProgress,
        BSOK
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BackupStatus = BackupStatus' Lude.Text
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

pattern BSDeleting :: BackupStatus
pattern BSDeleting = BackupStatus' "DELETING"

pattern BSFailed :: BackupStatus
pattern BSFailed = BackupStatus' "FAILED"

pattern BSInProgress :: BackupStatus
pattern BSInProgress = BackupStatus' "IN_PROGRESS"

pattern BSOK :: BackupStatus
pattern BSOK = BackupStatus' "OK"

{-# COMPLETE
  BSDeleting,
  BSFailed,
  BSInProgress,
  BSOK,
  BackupStatus'
  #-}
