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
-- Module      : Amazonka.MediaConvert.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264GopSizeUnits
  ( H264GopSizeUnits
      ( ..,
        H264GopSizeUnits_AUTO,
        H264GopSizeUnits_FRAMES,
        H264GopSizeUnits_SECONDS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specify how the transcoder determines GOP size for this output. We
-- recommend that you have the transcoder automatically choose this value
-- for you based on characteristics of your input video. To enable this
-- automatic behavior, choose Auto (AUTO) and and leave GOP size (GopSize)
-- blank. By default, if you don\'t specify GOP mode control
-- (GopSizeUnits), MediaConvert will use automatic behavior. If your output
-- group specifies HLS, DASH, or CMAF, set GOP mode control to Auto and
-- leave GOP size blank in each output in your output group. To explicitly
-- specify the GOP length, choose Specified, frames (FRAMES) or Specified,
-- seconds (SECONDS) and then provide the GOP length in the related setting
-- GOP size (GopSize).
newtype H264GopSizeUnits = H264GopSizeUnits'
  { fromH264GopSizeUnits ::
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

pattern H264GopSizeUnits_AUTO :: H264GopSizeUnits
pattern H264GopSizeUnits_AUTO = H264GopSizeUnits' "AUTO"

pattern H264GopSizeUnits_FRAMES :: H264GopSizeUnits
pattern H264GopSizeUnits_FRAMES = H264GopSizeUnits' "FRAMES"

pattern H264GopSizeUnits_SECONDS :: H264GopSizeUnits
pattern H264GopSizeUnits_SECONDS = H264GopSizeUnits' "SECONDS"

{-# COMPLETE
  H264GopSizeUnits_AUTO,
  H264GopSizeUnits_FRAMES,
  H264GopSizeUnits_SECONDS,
  H264GopSizeUnits'
  #-}
