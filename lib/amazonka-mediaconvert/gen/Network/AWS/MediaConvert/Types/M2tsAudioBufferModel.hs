{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsAudioBufferModel
  ( M2tsAudioBufferModel
      ( M2tsAudioBufferModel',
        M2tsAudioBufferModelDvb,
        M2tsAudioBufferModelAtsc,
        fromM2tsAudioBufferModel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
newtype M2tsAudioBufferModel = M2tsAudioBufferModel'
  { fromM2tsAudioBufferModel ::
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

pattern M2tsAudioBufferModelDvb :: M2tsAudioBufferModel
pattern M2tsAudioBufferModelDvb = M2tsAudioBufferModel' "DVB"

pattern M2tsAudioBufferModelAtsc :: M2tsAudioBufferModel
pattern M2tsAudioBufferModelAtsc = M2tsAudioBufferModel' "ATSC"

{-# COMPLETE
  M2tsAudioBufferModelDvb,
  M2tsAudioBufferModelAtsc,
  M2tsAudioBufferModel'
  #-}
