{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionExecutionStatus
  ( ActionExecutionStatus
    ( ActionExecutionStatus'
    , ActionExecutionStatusInProgress
    , ActionExecutionStatusAbandoned
    , ActionExecutionStatusSucceeded
    , ActionExecutionStatusFailed
    , fromActionExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActionExecutionStatus = ActionExecutionStatus'{fromActionExecutionStatus
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ActionExecutionStatusInProgress :: ActionExecutionStatus
pattern ActionExecutionStatusInProgress = ActionExecutionStatus' "InProgress"

pattern ActionExecutionStatusAbandoned :: ActionExecutionStatus
pattern ActionExecutionStatusAbandoned = ActionExecutionStatus' "Abandoned"

pattern ActionExecutionStatusSucceeded :: ActionExecutionStatus
pattern ActionExecutionStatusSucceeded = ActionExecutionStatus' "Succeeded"

pattern ActionExecutionStatusFailed :: ActionExecutionStatus
pattern ActionExecutionStatusFailed = ActionExecutionStatus' "Failed"

{-# COMPLETE 
  ActionExecutionStatusInProgress,

  ActionExecutionStatusAbandoned,

  ActionExecutionStatusSucceeded,

  ActionExecutionStatusFailed,
  ActionExecutionStatus'
  #-}
