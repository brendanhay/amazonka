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
-- Module      : Amazonka.MediaConvert.Types.M2tsEbpPlacement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.M2tsEbpPlacement
  ( M2tsEbpPlacement
      ( ..,
        M2tsEbpPlacement_VIDEO_AND_AUDIO_PIDS,
        M2tsEbpPlacement_VIDEO_PID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Selects which PIDs to place EBP markers on. They can either be placed
-- only on the video PID, or on both the video PID and all audio PIDs. Only
-- applicable when EBP segmentation markers are is selected
-- (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpPlacement = M2tsEbpPlacement'
  { fromM2tsEbpPlacement ::
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

pattern M2tsEbpPlacement_VIDEO_AND_AUDIO_PIDS :: M2tsEbpPlacement
pattern M2tsEbpPlacement_VIDEO_AND_AUDIO_PIDS = M2tsEbpPlacement' "VIDEO_AND_AUDIO_PIDS"

pattern M2tsEbpPlacement_VIDEO_PID :: M2tsEbpPlacement
pattern M2tsEbpPlacement_VIDEO_PID = M2tsEbpPlacement' "VIDEO_PID"

{-# COMPLETE
  M2tsEbpPlacement_VIDEO_AND_AUDIO_PIDS,
  M2tsEbpPlacement_VIDEO_PID,
  M2tsEbpPlacement'
  #-}
