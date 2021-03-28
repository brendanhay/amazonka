{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.ModelStatus
  ( ModelStatus
    ( ModelStatus'
    , ModelStatusSubmitted
    , ModelStatusTraining
    , ModelStatusDeleting
    , ModelStatusStopRequested
    , ModelStatusStopped
    , ModelStatusInError
    , ModelStatusTrained
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

pattern ModelStatusSubmitted :: ModelStatus
pattern ModelStatusSubmitted = ModelStatus' "SUBMITTED"

pattern ModelStatusTraining :: ModelStatus
pattern ModelStatusTraining = ModelStatus' "TRAINING"

pattern ModelStatusDeleting :: ModelStatus
pattern ModelStatusDeleting = ModelStatus' "DELETING"

pattern ModelStatusStopRequested :: ModelStatus
pattern ModelStatusStopRequested = ModelStatus' "STOP_REQUESTED"

pattern ModelStatusStopped :: ModelStatus
pattern ModelStatusStopped = ModelStatus' "STOPPED"

pattern ModelStatusInError :: ModelStatus
pattern ModelStatusInError = ModelStatus' "IN_ERROR"

pattern ModelStatusTrained :: ModelStatus
pattern ModelStatusTrained = ModelStatus' "TRAINED"

{-# COMPLETE 
  ModelStatusSubmitted,

  ModelStatusTraining,

  ModelStatusDeleting,

  ModelStatusStopRequested,

  ModelStatusStopped,

  ModelStatusInError,

  ModelStatusTrained,
  ModelStatus'
  #-}
