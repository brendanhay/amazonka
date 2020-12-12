{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ContinuousExportStatus
  ( ContinuousExportStatus
      ( ContinuousExportStatus',
        Active,
        Error,
        Inactive,
        StartFailed,
        StartInProgress,
        StopFailed,
        StopInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContinuousExportStatus = ContinuousExportStatus' Lude.Text
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

pattern Active :: ContinuousExportStatus
pattern Active = ContinuousExportStatus' "ACTIVE"

pattern Error :: ContinuousExportStatus
pattern Error = ContinuousExportStatus' "ERROR"

pattern Inactive :: ContinuousExportStatus
pattern Inactive = ContinuousExportStatus' "INACTIVE"

pattern StartFailed :: ContinuousExportStatus
pattern StartFailed = ContinuousExportStatus' "START_FAILED"

pattern StartInProgress :: ContinuousExportStatus
pattern StartInProgress = ContinuousExportStatus' "START_IN_PROGRESS"

pattern StopFailed :: ContinuousExportStatus
pattern StopFailed = ContinuousExportStatus' "STOP_FAILED"

pattern StopInProgress :: ContinuousExportStatus
pattern StopInProgress = ContinuousExportStatus' "STOP_IN_PROGRESS"

{-# COMPLETE
  Active,
  Error,
  Inactive,
  StartFailed,
  StartInProgress,
  StopFailed,
  StopInProgress,
  ContinuousExportStatus'
  #-}
