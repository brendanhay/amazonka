{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandFilterKey
  ( CommandFilterKey
    ( CommandFilterKey'
    , CommandFilterKeyCommandInvokedAfter
    , CommandFilterKeyCommandInvokedBefore
    , CommandFilterKeyCommandStatus
    , CommandFilterKeyCommandExecutionStage
    , CommandFilterKeyCommandDocumentName
    , fromCommandFilterKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CommandFilterKey = CommandFilterKey'{fromCommandFilterKey
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern CommandFilterKeyCommandInvokedAfter :: CommandFilterKey
pattern CommandFilterKeyCommandInvokedAfter = CommandFilterKey' "InvokedAfter"

pattern CommandFilterKeyCommandInvokedBefore :: CommandFilterKey
pattern CommandFilterKeyCommandInvokedBefore = CommandFilterKey' "InvokedBefore"

pattern CommandFilterKeyCommandStatus :: CommandFilterKey
pattern CommandFilterKeyCommandStatus = CommandFilterKey' "Status"

pattern CommandFilterKeyCommandExecutionStage :: CommandFilterKey
pattern CommandFilterKeyCommandExecutionStage = CommandFilterKey' "ExecutionStage"

pattern CommandFilterKeyCommandDocumentName :: CommandFilterKey
pattern CommandFilterKeyCommandDocumentName = CommandFilterKey' "DocumentName"

{-# COMPLETE 
  CommandFilterKeyCommandInvokedAfter,

  CommandFilterKeyCommandInvokedBefore,

  CommandFilterKeyCommandStatus,

  CommandFilterKeyCommandExecutionStage,

  CommandFilterKeyCommandDocumentName,
  CommandFilterKey'
  #-}
