{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.DeletionTaskStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.DeletionTaskStatusType
  ( DeletionTaskStatusType
      ( DeletionTaskStatusType',
        DTSTFailed,
        DTSTInProgress,
        DTSTNotStarted,
        DTSTSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeletionTaskStatusType = DeletionTaskStatusType' Lude.Text
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

pattern DTSTFailed :: DeletionTaskStatusType
pattern DTSTFailed = DeletionTaskStatusType' "FAILED"

pattern DTSTInProgress :: DeletionTaskStatusType
pattern DTSTInProgress = DeletionTaskStatusType' "IN_PROGRESS"

pattern DTSTNotStarted :: DeletionTaskStatusType
pattern DTSTNotStarted = DeletionTaskStatusType' "NOT_STARTED"

pattern DTSTSucceeded :: DeletionTaskStatusType
pattern DTSTSucceeded = DeletionTaskStatusType' "SUCCEEDED"

{-# COMPLETE
  DTSTFailed,
  DTSTInProgress,
  DTSTNotStarted,
  DTSTSucceeded,
  DeletionTaskStatusType'
  #-}
