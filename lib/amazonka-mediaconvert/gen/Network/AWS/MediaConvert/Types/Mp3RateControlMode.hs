{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3RateControlMode
  ( Mp3RateControlMode
      ( Mp3RateControlMode',
        Mp3RateControlModeCbr,
        Mp3RateControlModeVbr,
        fromMp3RateControlMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
newtype Mp3RateControlMode = Mp3RateControlMode'
  { fromMp3RateControlMode ::
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

pattern Mp3RateControlModeCbr :: Mp3RateControlMode
pattern Mp3RateControlModeCbr = Mp3RateControlMode' "CBR"

pattern Mp3RateControlModeVbr :: Mp3RateControlMode
pattern Mp3RateControlModeVbr = Mp3RateControlMode' "VBR"

{-# COMPLETE
  Mp3RateControlModeCbr,
  Mp3RateControlModeVbr,
  Mp3RateControlMode'
  #-}
