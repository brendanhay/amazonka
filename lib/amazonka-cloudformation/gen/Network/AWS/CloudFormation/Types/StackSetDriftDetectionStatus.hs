{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
  ( StackSetDriftDetectionStatus
      ( StackSetDriftDetectionStatus',
        SSDDSCompleted,
        SSDDSFailed,
        SSDDSPartialSuccess,
        SSDDSInProgress,
        SSDDSStopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackSetDriftDetectionStatus = StackSetDriftDetectionStatus' Lude.Text
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

pattern SSDDSCompleted :: StackSetDriftDetectionStatus
pattern SSDDSCompleted = StackSetDriftDetectionStatus' "COMPLETED"

pattern SSDDSFailed :: StackSetDriftDetectionStatus
pattern SSDDSFailed = StackSetDriftDetectionStatus' "FAILED"

pattern SSDDSPartialSuccess :: StackSetDriftDetectionStatus
pattern SSDDSPartialSuccess = StackSetDriftDetectionStatus' "PARTIAL_SUCCESS"

pattern SSDDSInProgress :: StackSetDriftDetectionStatus
pattern SSDDSInProgress = StackSetDriftDetectionStatus' "IN_PROGRESS"

pattern SSDDSStopped :: StackSetDriftDetectionStatus
pattern SSDDSStopped = StackSetDriftDetectionStatus' "STOPPED"

{-# COMPLETE
  SSDDSCompleted,
  SSDDSFailed,
  SSDDSPartialSuccess,
  SSDDSInProgress,
  SSDDSStopped,
  StackSetDriftDetectionStatus'
  #-}
