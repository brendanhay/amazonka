{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionStatus
  ( ActionExecutionStatus
      ( ..,
        ActionExecutionStatus_Abandoned,
        ActionExecutionStatus_Failed,
        ActionExecutionStatus_InProgress,
        ActionExecutionStatus_Succeeded
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionExecutionStatus = ActionExecutionStatus'
  { fromActionExecutionStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ActionExecutionStatus_Abandoned :: ActionExecutionStatus
pattern ActionExecutionStatus_Abandoned = ActionExecutionStatus' "Abandoned"

pattern ActionExecutionStatus_Failed :: ActionExecutionStatus
pattern ActionExecutionStatus_Failed = ActionExecutionStatus' "Failed"

pattern ActionExecutionStatus_InProgress :: ActionExecutionStatus
pattern ActionExecutionStatus_InProgress = ActionExecutionStatus' "InProgress"

pattern ActionExecutionStatus_Succeeded :: ActionExecutionStatus
pattern ActionExecutionStatus_Succeeded = ActionExecutionStatus' "Succeeded"

{-# COMPLETE
  ActionExecutionStatus_Abandoned,
  ActionExecutionStatus_Failed,
  ActionExecutionStatus_InProgress,
  ActionExecutionStatus_Succeeded,
  ActionExecutionStatus'
  #-}
