{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ShareStatus
  ( ShareStatus
    ( ShareStatus'
    , ShareStatusNotStarted
    , ShareStatusInProgress
    , ShareStatusCompleted
    , ShareStatusCompletedWithErrors
    , ShareStatusError
    , fromShareStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ShareStatus = ShareStatus'{fromShareStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ShareStatusNotStarted :: ShareStatus
pattern ShareStatusNotStarted = ShareStatus' "NOT_STARTED"

pattern ShareStatusInProgress :: ShareStatus
pattern ShareStatusInProgress = ShareStatus' "IN_PROGRESS"

pattern ShareStatusCompleted :: ShareStatus
pattern ShareStatusCompleted = ShareStatus' "COMPLETED"

pattern ShareStatusCompletedWithErrors :: ShareStatus
pattern ShareStatusCompletedWithErrors = ShareStatus' "COMPLETED_WITH_ERRORS"

pattern ShareStatusError :: ShareStatus
pattern ShareStatusError = ShareStatus' "ERROR"

{-# COMPLETE 
  ShareStatusNotStarted,

  ShareStatusInProgress,

  ShareStatusCompleted,

  ShareStatusCompletedWithErrors,

  ShareStatusError,
  ShareStatus'
  #-}
