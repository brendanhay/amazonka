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
        Dvb,
        Atsc
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
newtype M2tsAudioBufferModel = M2tsAudioBufferModel' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Dvb :: M2tsAudioBufferModel
pattern Dvb = M2tsAudioBufferModel' "DVB"

pattern Atsc :: M2tsAudioBufferModel
pattern Atsc = M2tsAudioBufferModel' "ATSC"

{-# COMPLETE
  Dvb,
  Atsc,
  M2tsAudioBufferModel'
  #-}
