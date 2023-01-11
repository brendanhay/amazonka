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
-- Module      : Amazonka.MediaConvert.Types.SccDestinationFramerate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SccDestinationFramerate
  ( SccDestinationFramerate
      ( ..,
        SccDestinationFramerate_FRAMERATE_23_97,
        SccDestinationFramerate_FRAMERATE_24,
        SccDestinationFramerate_FRAMERATE_25,
        SccDestinationFramerate_FRAMERATE_29_97_DROPFRAME,
        SccDestinationFramerate_FRAMERATE_29_97_NON_DROPFRAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions
-- and the video are synchronized in the output. Specify a frame rate that
-- matches the frame rate of the associated video. If the video frame rate
-- is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the
-- video has video_insertion=true and drop_frame_timecode=true; otherwise,
-- choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
newtype SccDestinationFramerate = SccDestinationFramerate'
  { fromSccDestinationFramerate ::
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

pattern SccDestinationFramerate_FRAMERATE_23_97 :: SccDestinationFramerate
pattern SccDestinationFramerate_FRAMERATE_23_97 = SccDestinationFramerate' "FRAMERATE_23_97"

pattern SccDestinationFramerate_FRAMERATE_24 :: SccDestinationFramerate
pattern SccDestinationFramerate_FRAMERATE_24 = SccDestinationFramerate' "FRAMERATE_24"

pattern SccDestinationFramerate_FRAMERATE_25 :: SccDestinationFramerate
pattern SccDestinationFramerate_FRAMERATE_25 = SccDestinationFramerate' "FRAMERATE_25"

pattern SccDestinationFramerate_FRAMERATE_29_97_DROPFRAME :: SccDestinationFramerate
pattern SccDestinationFramerate_FRAMERATE_29_97_DROPFRAME = SccDestinationFramerate' "FRAMERATE_29_97_DROPFRAME"

pattern SccDestinationFramerate_FRAMERATE_29_97_NON_DROPFRAME :: SccDestinationFramerate
pattern SccDestinationFramerate_FRAMERATE_29_97_NON_DROPFRAME = SccDestinationFramerate' "FRAMERATE_29_97_NON_DROPFRAME"

{-# COMPLETE
  SccDestinationFramerate_FRAMERATE_23_97,
  SccDestinationFramerate_FRAMERATE_24,
  SccDestinationFramerate_FRAMERATE_25,
  SccDestinationFramerate_FRAMERATE_29_97_DROPFRAME,
  SccDestinationFramerate_FRAMERATE_29_97_NON_DROPFRAME,
  SccDestinationFramerate'
  #-}
