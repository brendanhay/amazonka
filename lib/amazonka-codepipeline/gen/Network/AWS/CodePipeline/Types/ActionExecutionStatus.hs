{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionStatus
  ( ActionExecutionStatus
      ( ActionExecutionStatus',
        AESInProgress,
        AESAbandoned,
        AESSucceeded,
        AESFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionExecutionStatus = ActionExecutionStatus' Lude.Text
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

pattern AESInProgress :: ActionExecutionStatus
pattern AESInProgress = ActionExecutionStatus' "InProgress"

pattern AESAbandoned :: ActionExecutionStatus
pattern AESAbandoned = ActionExecutionStatus' "Abandoned"

pattern AESSucceeded :: ActionExecutionStatus
pattern AESSucceeded = ActionExecutionStatus' "Succeeded"

pattern AESFailed :: ActionExecutionStatus
pattern AESFailed = ActionExecutionStatus' "Failed"

{-# COMPLETE
  AESInProgress,
  AESAbandoned,
  AESSucceeded,
  AESFailed,
  ActionExecutionStatus'
  #-}
