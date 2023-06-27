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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatus
  ( MediaPipelineStatus
      ( ..,
        MediaPipelineStatus_Failed,
        MediaPipelineStatus_InProgress,
        MediaPipelineStatus_Initializing,
        MediaPipelineStatus_Paused,
        MediaPipelineStatus_Stopped,
        MediaPipelineStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MediaPipelineStatus = MediaPipelineStatus'
  { fromMediaPipelineStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern MediaPipelineStatus_Failed :: MediaPipelineStatus
pattern MediaPipelineStatus_Failed = MediaPipelineStatus' "Failed"

pattern MediaPipelineStatus_InProgress :: MediaPipelineStatus
pattern MediaPipelineStatus_InProgress = MediaPipelineStatus' "InProgress"

pattern MediaPipelineStatus_Initializing :: MediaPipelineStatus
pattern MediaPipelineStatus_Initializing = MediaPipelineStatus' "Initializing"

pattern MediaPipelineStatus_Paused :: MediaPipelineStatus
pattern MediaPipelineStatus_Paused = MediaPipelineStatus' "Paused"

pattern MediaPipelineStatus_Stopped :: MediaPipelineStatus
pattern MediaPipelineStatus_Stopped = MediaPipelineStatus' "Stopped"

pattern MediaPipelineStatus_Stopping :: MediaPipelineStatus
pattern MediaPipelineStatus_Stopping = MediaPipelineStatus' "Stopping"

{-# COMPLETE
  MediaPipelineStatus_Failed,
  MediaPipelineStatus_InProgress,
  MediaPipelineStatus_Initializing,
  MediaPipelineStatus_Paused,
  MediaPipelineStatus_Stopped,
  MediaPipelineStatus_Stopping,
  MediaPipelineStatus'
  #-}
