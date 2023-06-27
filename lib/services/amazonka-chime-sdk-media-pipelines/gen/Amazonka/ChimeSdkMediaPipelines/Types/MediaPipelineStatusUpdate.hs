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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatusUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaPipelineStatusUpdate
  ( MediaPipelineStatusUpdate
      ( ..,
        MediaPipelineStatusUpdate_Pause,
        MediaPipelineStatusUpdate_Resume
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MediaPipelineStatusUpdate = MediaPipelineStatusUpdate'
  { fromMediaPipelineStatusUpdate ::
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

pattern MediaPipelineStatusUpdate_Pause :: MediaPipelineStatusUpdate
pattern MediaPipelineStatusUpdate_Pause = MediaPipelineStatusUpdate' "Pause"

pattern MediaPipelineStatusUpdate_Resume :: MediaPipelineStatusUpdate
pattern MediaPipelineStatusUpdate_Resume = MediaPipelineStatusUpdate' "Resume"

{-# COMPLETE
  MediaPipelineStatusUpdate_Pause,
  MediaPipelineStatusUpdate_Resume,
  MediaPipelineStatusUpdate'
  #-}
