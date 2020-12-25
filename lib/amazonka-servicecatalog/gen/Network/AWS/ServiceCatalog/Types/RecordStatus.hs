{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordStatus
  ( RecordStatus
      ( RecordStatus',
        RecordStatusCreated,
        RecordStatusInProgress,
        RecordStatusInProgressInError,
        RecordStatusSucceeded,
        RecordStatusFailed,
        fromRecordStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RecordStatus = RecordStatus' {fromRecordStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern RecordStatusCreated :: RecordStatus
pattern RecordStatusCreated = RecordStatus' "CREATED"

pattern RecordStatusInProgress :: RecordStatus
pattern RecordStatusInProgress = RecordStatus' "IN_PROGRESS"

pattern RecordStatusInProgressInError :: RecordStatus
pattern RecordStatusInProgressInError = RecordStatus' "IN_PROGRESS_IN_ERROR"

pattern RecordStatusSucceeded :: RecordStatus
pattern RecordStatusSucceeded = RecordStatus' "SUCCEEDED"

pattern RecordStatusFailed :: RecordStatus
pattern RecordStatusFailed = RecordStatus' "FAILED"

{-# COMPLETE
  RecordStatusCreated,
  RecordStatusInProgress,
  RecordStatusInProgressInError,
  RecordStatusSucceeded,
  RecordStatusFailed,
  RecordStatus'
  #-}
