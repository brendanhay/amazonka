{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
  ( H264UnregisteredSeiTimecode
      ( H264UnregisteredSeiTimecode',
        H264UnregisteredSeiTimecodeDisabled,
        H264UnregisteredSeiTimecodeEnabled,
        fromH264UnregisteredSeiTimecode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode'
  { fromH264UnregisteredSeiTimecode ::
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

pattern H264UnregisteredSeiTimecodeDisabled :: H264UnregisteredSeiTimecode
pattern H264UnregisteredSeiTimecodeDisabled = H264UnregisteredSeiTimecode' "DISABLED"

pattern H264UnregisteredSeiTimecodeEnabled :: H264UnregisteredSeiTimecode
pattern H264UnregisteredSeiTimecodeEnabled = H264UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  H264UnregisteredSeiTimecodeDisabled,
  H264UnregisteredSeiTimecodeEnabled,
  H264UnregisteredSeiTimecode'
  #-}
