{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Source
  ( MpdScte35Source
      ( MpdScte35Source',
        MpdScte35SourcePassthrough,
        MpdScte35SourceNone,
        fromMpdScte35Source
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
newtype MpdScte35Source = MpdScte35Source'
  { fromMpdScte35Source ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern MpdScte35SourcePassthrough :: MpdScte35Source
pattern MpdScte35SourcePassthrough = MpdScte35Source' "PASSTHROUGH"

pattern MpdScte35SourceNone :: MpdScte35Source
pattern MpdScte35SourceNone = MpdScte35Source' "NONE"

{-# COMPLETE
  MpdScte35SourcePassthrough,
  MpdScte35SourceNone,
  MpdScte35Source'
  #-}
