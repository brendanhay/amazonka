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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobStatus
  ( AutoMLJobStatus
      ( ..,
        AutoMLJobStatus_Completed,
        AutoMLJobStatus_Failed,
        AutoMLJobStatus_InProgress,
        AutoMLJobStatus_Stopped,
        AutoMLJobStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoMLJobStatus = AutoMLJobStatus'
  { fromAutoMLJobStatus ::
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

pattern AutoMLJobStatus_Completed :: AutoMLJobStatus
pattern AutoMLJobStatus_Completed = AutoMLJobStatus' "Completed"

pattern AutoMLJobStatus_Failed :: AutoMLJobStatus
pattern AutoMLJobStatus_Failed = AutoMLJobStatus' "Failed"

pattern AutoMLJobStatus_InProgress :: AutoMLJobStatus
pattern AutoMLJobStatus_InProgress = AutoMLJobStatus' "InProgress"

pattern AutoMLJobStatus_Stopped :: AutoMLJobStatus
pattern AutoMLJobStatus_Stopped = AutoMLJobStatus' "Stopped"

pattern AutoMLJobStatus_Stopping :: AutoMLJobStatus
pattern AutoMLJobStatus_Stopping = AutoMLJobStatus' "Stopping"

{-# COMPLETE
  AutoMLJobStatus_Completed,
  AutoMLJobStatus_Failed,
  AutoMLJobStatus_InProgress,
  AutoMLJobStatus_Stopped,
  AutoMLJobStatus_Stopping,
  AutoMLJobStatus'
  #-}
