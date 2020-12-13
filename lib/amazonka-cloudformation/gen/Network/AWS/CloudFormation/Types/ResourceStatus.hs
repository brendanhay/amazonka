{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceStatus
  ( ResourceStatus
      ( ResourceStatus',
        CreateInProgress,
        CreateFailed,
        CreateComplete,
        DeleteInProgress,
        DeleteFailed,
        DeleteComplete,
        DeleteSkipped,
        UpdateInProgress,
        UpdateFailed,
        UpdateComplete,
        ImportFailed,
        ImportComplete,
        ImportInProgress,
        ImportRollbackInProgress,
        ImportRollbackFailed,
        ImportRollbackComplete
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceStatus = ResourceStatus' Lude.Text
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

pattern CreateInProgress :: ResourceStatus
pattern CreateInProgress = ResourceStatus' "CREATE_IN_PROGRESS"

pattern CreateFailed :: ResourceStatus
pattern CreateFailed = ResourceStatus' "CREATE_FAILED"

pattern CreateComplete :: ResourceStatus
pattern CreateComplete = ResourceStatus' "CREATE_COMPLETE"

pattern DeleteInProgress :: ResourceStatus
pattern DeleteInProgress = ResourceStatus' "DELETE_IN_PROGRESS"

pattern DeleteFailed :: ResourceStatus
pattern DeleteFailed = ResourceStatus' "DELETE_FAILED"

pattern DeleteComplete :: ResourceStatus
pattern DeleteComplete = ResourceStatus' "DELETE_COMPLETE"

pattern DeleteSkipped :: ResourceStatus
pattern DeleteSkipped = ResourceStatus' "DELETE_SKIPPED"

pattern UpdateInProgress :: ResourceStatus
pattern UpdateInProgress = ResourceStatus' "UPDATE_IN_PROGRESS"

pattern UpdateFailed :: ResourceStatus
pattern UpdateFailed = ResourceStatus' "UPDATE_FAILED"

pattern UpdateComplete :: ResourceStatus
pattern UpdateComplete = ResourceStatus' "UPDATE_COMPLETE"

pattern ImportFailed :: ResourceStatus
pattern ImportFailed = ResourceStatus' "IMPORT_FAILED"

pattern ImportComplete :: ResourceStatus
pattern ImportComplete = ResourceStatus' "IMPORT_COMPLETE"

pattern ImportInProgress :: ResourceStatus
pattern ImportInProgress = ResourceStatus' "IMPORT_IN_PROGRESS"

pattern ImportRollbackInProgress :: ResourceStatus
pattern ImportRollbackInProgress = ResourceStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern ImportRollbackFailed :: ResourceStatus
pattern ImportRollbackFailed = ResourceStatus' "IMPORT_ROLLBACK_FAILED"

pattern ImportRollbackComplete :: ResourceStatus
pattern ImportRollbackComplete = ResourceStatus' "IMPORT_ROLLBACK_COMPLETE"

{-# COMPLETE
  CreateInProgress,
  CreateFailed,
  CreateComplete,
  DeleteInProgress,
  DeleteFailed,
  DeleteComplete,
  DeleteSkipped,
  UpdateInProgress,
  UpdateFailed,
  UpdateComplete,
  ImportFailed,
  ImportComplete,
  ImportInProgress,
  ImportRollbackInProgress,
  ImportRollbackFailed,
  ImportRollbackComplete,
  ResourceStatus'
  #-}
