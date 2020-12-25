{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportStatus
  ( ExportStatus
      ( ExportStatus',
        ExportStatusFailed,
        ExportStatusSucceeded,
        ExportStatusInProgress,
        fromExportStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ExportStatus = ExportStatus' {fromExportStatus :: Core.Text}
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

pattern ExportStatusFailed :: ExportStatus
pattern ExportStatusFailed = ExportStatus' "FAILED"

pattern ExportStatusSucceeded :: ExportStatus
pattern ExportStatusSucceeded = ExportStatus' "SUCCEEDED"

pattern ExportStatusInProgress :: ExportStatus
pattern ExportStatusInProgress = ExportStatus' "IN_PROGRESS"

{-# COMPLETE
  ExportStatusFailed,
  ExportStatusSucceeded,
  ExportStatusInProgress,
  ExportStatus'
  #-}
