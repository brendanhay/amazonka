-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ApplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ApplicationStatus
  ( ApplicationStatus
      ( ApplicationStatus',
        Completed,
        InProgress,
        NotStarted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ApplicationStatus = ApplicationStatus' Lude.Text
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

pattern Completed :: ApplicationStatus
pattern Completed = ApplicationStatus' "COMPLETED"

pattern InProgress :: ApplicationStatus
pattern InProgress = ApplicationStatus' "IN_PROGRESS"

pattern NotStarted :: ApplicationStatus
pattern NotStarted = ApplicationStatus' "NOT_STARTED"

{-# COMPLETE
  Completed,
  InProgress,
  NotStarted,
  ApplicationStatus'
  #-}
