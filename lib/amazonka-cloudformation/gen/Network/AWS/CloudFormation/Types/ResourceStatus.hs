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
        CreateComplete,
        CreateFailed,
        CreateInProgress,
        DeleteComplete,
        DeleteFailed,
        DeleteInProgress,
        DeleteSkipped,
        ImportComplete,
        ImportFailed,
        ImportInProgress,
        ImportRollbackComplete,
        ImportRollbackFailed,
        ImportRollbackInProgress,
        UpdateComplete,
        UpdateFailed,
        UpdateInProgress
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

pattern CreateComplete :: ResourceStatus
pattern CreateComplete = ResourceStatus' "CREATE_COMPLETE"

pattern CreateFailed :: ResourceStatus
pattern CreateFailed = ResourceStatus' "CREATE_FAILED"

pattern CreateInProgress :: ResourceStatus
pattern CreateInProgress = ResourceStatus' "CREATE_IN_PROGRESS"

pattern DeleteComplete :: ResourceStatus
pattern DeleteComplete = ResourceStatus' "DELETE_COMPLETE"

pattern DeleteFailed :: ResourceStatus
pattern DeleteFailed = ResourceStatus' "DELETE_FAILED"

pattern DeleteInProgress :: ResourceStatus
pattern DeleteInProgress = ResourceStatus' "DELETE_IN_PROGRESS"

pattern DeleteSkipped :: ResourceStatus
pattern DeleteSkipped = ResourceStatus' "DELETE_SKIPPED"

pattern ImportComplete :: ResourceStatus
pattern ImportComplete = ResourceStatus' "IMPORT_COMPLETE"

pattern ImportFailed :: ResourceStatus
pattern ImportFailed = ResourceStatus' "IMPORT_FAILED"

pattern ImportInProgress :: ResourceStatus
pattern ImportInProgress = ResourceStatus' "IMPORT_IN_PROGRESS"

pattern ImportRollbackComplete :: ResourceStatus
pattern ImportRollbackComplete = ResourceStatus' "IMPORT_ROLLBACK_COMPLETE"

pattern ImportRollbackFailed :: ResourceStatus
pattern ImportRollbackFailed = ResourceStatus' "IMPORT_ROLLBACK_FAILED"

pattern ImportRollbackInProgress :: ResourceStatus
pattern ImportRollbackInProgress = ResourceStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern UpdateComplete :: ResourceStatus
pattern UpdateComplete = ResourceStatus' "UPDATE_COMPLETE"

pattern UpdateFailed :: ResourceStatus
pattern UpdateFailed = ResourceStatus' "UPDATE_FAILED"

pattern UpdateInProgress :: ResourceStatus
pattern UpdateInProgress = ResourceStatus' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  CreateComplete,
  CreateFailed,
  CreateInProgress,
  DeleteComplete,
  DeleteFailed,
  DeleteInProgress,
  DeleteSkipped,
  ImportComplete,
  ImportFailed,
  ImportInProgress,
  ImportRollbackComplete,
  ImportRollbackFailed,
  ImportRollbackInProgress,
  UpdateComplete,
  UpdateFailed,
  UpdateInProgress,
  ResourceStatus'
  #-}
