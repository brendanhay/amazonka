{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceStatus
  ( NotebookInstanceStatus
    ( NotebookInstanceStatus'
    , NotebookInstanceStatusPending
    , NotebookInstanceStatusInService
    , NotebookInstanceStatusStopping
    , NotebookInstanceStatusStopped
    , NotebookInstanceStatusFailed
    , NotebookInstanceStatusDeleting
    , NotebookInstanceStatusUpdating
    , fromNotebookInstanceStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NotebookInstanceStatus = NotebookInstanceStatus'{fromNotebookInstanceStatus
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern NotebookInstanceStatusPending :: NotebookInstanceStatus
pattern NotebookInstanceStatusPending = NotebookInstanceStatus' "Pending"

pattern NotebookInstanceStatusInService :: NotebookInstanceStatus
pattern NotebookInstanceStatusInService = NotebookInstanceStatus' "InService"

pattern NotebookInstanceStatusStopping :: NotebookInstanceStatus
pattern NotebookInstanceStatusStopping = NotebookInstanceStatus' "Stopping"

pattern NotebookInstanceStatusStopped :: NotebookInstanceStatus
pattern NotebookInstanceStatusStopped = NotebookInstanceStatus' "Stopped"

pattern NotebookInstanceStatusFailed :: NotebookInstanceStatus
pattern NotebookInstanceStatusFailed = NotebookInstanceStatus' "Failed"

pattern NotebookInstanceStatusDeleting :: NotebookInstanceStatus
pattern NotebookInstanceStatusDeleting = NotebookInstanceStatus' "Deleting"

pattern NotebookInstanceStatusUpdating :: NotebookInstanceStatus
pattern NotebookInstanceStatusUpdating = NotebookInstanceStatus' "Updating"

{-# COMPLETE 
  NotebookInstanceStatusPending,

  NotebookInstanceStatusInService,

  NotebookInstanceStatusStopping,

  NotebookInstanceStatusStopped,

  NotebookInstanceStatusFailed,

  NotebookInstanceStatusDeleting,

  NotebookInstanceStatusUpdating,
  NotebookInstanceStatus'
  #-}
