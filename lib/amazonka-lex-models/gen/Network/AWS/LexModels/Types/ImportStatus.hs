{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ImportStatus
  ( ImportStatus
      ( ImportStatus',
        ImportStatusInProgress,
        ImportStatusComplete,
        ImportStatusFailed,
        fromImportStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImportStatus = ImportStatus' {fromImportStatus :: Core.Text}
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

pattern ImportStatusInProgress :: ImportStatus
pattern ImportStatusInProgress = ImportStatus' "IN_PROGRESS"

pattern ImportStatusComplete :: ImportStatus
pattern ImportStatusComplete = ImportStatus' "COMPLETE"

pattern ImportStatusFailed :: ImportStatus
pattern ImportStatusFailed = ImportStatus' "FAILED"

{-# COMPLETE
  ImportStatusInProgress,
  ImportStatusComplete,
  ImportStatusFailed,
  ImportStatus'
  #-}
