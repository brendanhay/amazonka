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
-- Module      : Amazonka.Chime.Types.MediaPipelineStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.MediaPipelineStatus
  ( MediaPipelineStatus
      ( ..,
        MediaPipelineStatus_Failed,
        MediaPipelineStatus_InProgress,
        MediaPipelineStatus_Initializing,
        MediaPipelineStatus_Stopped,
        MediaPipelineStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MediaPipelineStatus = MediaPipelineStatus'
  { fromMediaPipelineStatus ::
      Core.Text
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

pattern MediaPipelineStatus_Failed :: MediaPipelineStatus
pattern MediaPipelineStatus_Failed = MediaPipelineStatus' "Failed"

pattern MediaPipelineStatus_InProgress :: MediaPipelineStatus
pattern MediaPipelineStatus_InProgress = MediaPipelineStatus' "InProgress"

pattern MediaPipelineStatus_Initializing :: MediaPipelineStatus
pattern MediaPipelineStatus_Initializing = MediaPipelineStatus' "Initializing"

pattern MediaPipelineStatus_Stopped :: MediaPipelineStatus
pattern MediaPipelineStatus_Stopped = MediaPipelineStatus' "Stopped"

pattern MediaPipelineStatus_Stopping :: MediaPipelineStatus
pattern MediaPipelineStatus_Stopping = MediaPipelineStatus' "Stopping"

{-# COMPLETE
  MediaPipelineStatus_Failed,
  MediaPipelineStatus_InProgress,
  MediaPipelineStatus_Initializing,
  MediaPipelineStatus_Stopped,
  MediaPipelineStatus_Stopping,
  MediaPipelineStatus'
  #-}
