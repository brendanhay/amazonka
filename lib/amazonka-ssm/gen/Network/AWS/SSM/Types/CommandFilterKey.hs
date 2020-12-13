{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandFilterKey
  ( CommandFilterKey
      ( CommandFilterKey',
        CommandInvokedAfter,
        CommandInvokedBefore,
        CommandStatus,
        CommandExecutionStage,
        CommandDocumentName
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CommandFilterKey = CommandFilterKey' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CommandInvokedAfter :: CommandFilterKey
pattern CommandInvokedAfter = CommandFilterKey' "InvokedAfter"

pattern CommandInvokedBefore :: CommandFilterKey
pattern CommandInvokedBefore = CommandFilterKey' "InvokedBefore"

pattern CommandStatus :: CommandFilterKey
pattern CommandStatus = CommandFilterKey' "Status"

pattern CommandExecutionStage :: CommandFilterKey
pattern CommandExecutionStage = CommandFilterKey' "ExecutionStage"

pattern CommandDocumentName :: CommandFilterKey
pattern CommandDocumentName = CommandFilterKey' "DocumentName"

{-# COMPLETE
  CommandInvokedAfter,
  CommandInvokedBefore,
  CommandStatus,
  CommandExecutionStage,
  CommandDocumentName,
  CommandFilterKey'
  #-}
