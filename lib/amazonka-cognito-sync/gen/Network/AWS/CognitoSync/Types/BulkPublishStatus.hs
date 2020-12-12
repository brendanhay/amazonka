{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.BulkPublishStatus
  ( BulkPublishStatus
      ( BulkPublishStatus',
        Failed,
        InProgress,
        NotStarted,
        Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BulkPublishStatus = BulkPublishStatus' Lude.Text
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

pattern Failed :: BulkPublishStatus
pattern Failed = BulkPublishStatus' "FAILED"

pattern InProgress :: BulkPublishStatus
pattern InProgress = BulkPublishStatus' "IN_PROGRESS"

pattern NotStarted :: BulkPublishStatus
pattern NotStarted = BulkPublishStatus' "NOT_STARTED"

pattern Succeeded :: BulkPublishStatus
pattern Succeeded = BulkPublishStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  NotStarted,
  Succeeded,
  BulkPublishStatus'
  #-}
