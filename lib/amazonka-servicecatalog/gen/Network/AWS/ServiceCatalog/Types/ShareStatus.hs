-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareStatus
  ( ShareStatus
      ( ShareStatus',
        Completed,
        CompletedWithErrors,
        Error,
        InProgress,
        NotStarted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShareStatus = ShareStatus' Lude.Text
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

pattern Completed :: ShareStatus
pattern Completed = ShareStatus' "COMPLETED"

pattern CompletedWithErrors :: ShareStatus
pattern CompletedWithErrors = ShareStatus' "COMPLETED_WITH_ERRORS"

pattern Error :: ShareStatus
pattern Error = ShareStatus' "ERROR"

pattern InProgress :: ShareStatus
pattern InProgress = ShareStatus' "IN_PROGRESS"

pattern NotStarted :: ShareStatus
pattern NotStarted = ShareStatus' "NOT_STARTED"

{-# COMPLETE
  Completed,
  CompletedWithErrors,
  Error,
  InProgress,
  NotStarted,
  ShareStatus'
  #-}
