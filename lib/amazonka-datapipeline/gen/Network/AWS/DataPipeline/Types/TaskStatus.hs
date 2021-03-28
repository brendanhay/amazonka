{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.TaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.TaskStatus
  ( TaskStatus
    ( TaskStatus'
    , TaskStatusFinished
    , TaskStatusFailed
    , TaskStatusFalse
    , fromTaskStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TaskStatus = TaskStatus'{fromTaskStatus :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern TaskStatusFinished :: TaskStatus
pattern TaskStatusFinished = TaskStatus' "FINISHED"

pattern TaskStatusFailed :: TaskStatus
pattern TaskStatusFailed = TaskStatus' "FAILED"

pattern TaskStatusFalse :: TaskStatus
pattern TaskStatusFalse = TaskStatus' "FALSE"

{-# COMPLETE 
  TaskStatusFinished,

  TaskStatusFailed,

  TaskStatusFalse,
  TaskStatus'
  #-}
