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
-- Module      : Amazonka.MediaConvert.Types.MxfAfdSignaling
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfAfdSignaling
  ( MxfAfdSignaling
      ( ..,
        MxfAfdSignaling_COPY_FROM_VIDEO,
        MxfAfdSignaling_NO_COPY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. When you have AFD signaling set up in your output video
-- stream, use this setting to choose whether to also include it in the MXF
-- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
-- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
-- AFD values from the video stream for this output to the MXF wrapper.
-- Regardless of which option you choose, the AFD values remain in the
-- video stream. Related settings: To set up your output to include or
-- exclude AFD values, see AfdSignaling, under VideoDescription. On the
-- console, find AFD signaling under the output\'s video encoding settings.
newtype MxfAfdSignaling = MxfAfdSignaling'
  { fromMxfAfdSignaling ::
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

pattern MxfAfdSignaling_COPY_FROM_VIDEO :: MxfAfdSignaling
pattern MxfAfdSignaling_COPY_FROM_VIDEO = MxfAfdSignaling' "COPY_FROM_VIDEO"

pattern MxfAfdSignaling_NO_COPY :: MxfAfdSignaling
pattern MxfAfdSignaling_NO_COPY = MxfAfdSignaling' "NO_COPY"

{-# COMPLETE
  MxfAfdSignaling_COPY_FROM_VIDEO,
  MxfAfdSignaling_NO_COPY,
  MxfAfdSignaling'
  #-}
