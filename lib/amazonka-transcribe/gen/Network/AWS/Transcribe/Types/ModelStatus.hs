{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ModelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.ModelStatus
  ( ModelStatus
    ( ModelStatus'
    , ModelStatusInProgress
    , ModelStatusFailed
    , ModelStatusCompleted
    , fromModelStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ModelStatus = ModelStatus'{fromModelStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ModelStatusInProgress :: ModelStatus
pattern ModelStatusInProgress = ModelStatus' "IN_PROGRESS"

pattern ModelStatusFailed :: ModelStatus
pattern ModelStatusFailed = ModelStatus' "FAILED"

pattern ModelStatusCompleted :: ModelStatus
pattern ModelStatusCompleted = ModelStatus' "COMPLETED"

{-# COMPLETE 
  ModelStatusInProgress,

  ModelStatusFailed,

  ModelStatusCompleted,
  ModelStatus'
  #-}
