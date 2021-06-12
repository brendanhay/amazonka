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
-- Module      : Network.AWS.SageMaker.Types.ActionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ActionStatus
  ( ActionStatus
      ( ..,
        ActionStatus_Completed,
        ActionStatus_Failed,
        ActionStatus_InProgress,
        ActionStatus_Stopped,
        ActionStatus_Stopping,
        ActionStatus_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionStatus = ActionStatus'
  { fromActionStatus ::
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

pattern ActionStatus_Completed :: ActionStatus
pattern ActionStatus_Completed = ActionStatus' "Completed"

pattern ActionStatus_Failed :: ActionStatus
pattern ActionStatus_Failed = ActionStatus' "Failed"

pattern ActionStatus_InProgress :: ActionStatus
pattern ActionStatus_InProgress = ActionStatus' "InProgress"

pattern ActionStatus_Stopped :: ActionStatus
pattern ActionStatus_Stopped = ActionStatus' "Stopped"

pattern ActionStatus_Stopping :: ActionStatus
pattern ActionStatus_Stopping = ActionStatus' "Stopping"

pattern ActionStatus_Unknown :: ActionStatus
pattern ActionStatus_Unknown = ActionStatus' "Unknown"

{-# COMPLETE
  ActionStatus_Completed,
  ActionStatus_Failed,
  ActionStatus_InProgress,
  ActionStatus_Stopped,
  ActionStatus_Stopping,
  ActionStatus_Unknown,
  ActionStatus'
  #-}
