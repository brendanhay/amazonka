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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2GopSizeUnits
  ( Mpeg2GopSizeUnits
      ( ..,
        Mpeg2GopSizeUnits_FRAMES,
        Mpeg2GopSizeUnits_SECONDS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
newtype Mpeg2GopSizeUnits = Mpeg2GopSizeUnits'
  { fromMpeg2GopSizeUnits ::
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

pattern Mpeg2GopSizeUnits_FRAMES :: Mpeg2GopSizeUnits
pattern Mpeg2GopSizeUnits_FRAMES = Mpeg2GopSizeUnits' "FRAMES"

pattern Mpeg2GopSizeUnits_SECONDS :: Mpeg2GopSizeUnits
pattern Mpeg2GopSizeUnits_SECONDS = Mpeg2GopSizeUnits' "SECONDS"

{-# COMPLETE
  Mpeg2GopSizeUnits_FRAMES,
  Mpeg2GopSizeUnits_SECONDS,
  Mpeg2GopSizeUnits'
  #-}
