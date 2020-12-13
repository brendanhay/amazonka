{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
  ( Eac3AtmosStereoDownmix
      ( Eac3AtmosStereoDownmix',
        EASDNotIndicated,
        EASDStereo,
        EASDSurround,
        EASDDPL2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose how the service does stereo downmixing.
newtype Eac3AtmosStereoDownmix = Eac3AtmosStereoDownmix' Lude.Text
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

pattern EASDNotIndicated :: Eac3AtmosStereoDownmix
pattern EASDNotIndicated = Eac3AtmosStereoDownmix' "NOT_INDICATED"

pattern EASDStereo :: Eac3AtmosStereoDownmix
pattern EASDStereo = Eac3AtmosStereoDownmix' "STEREO"

pattern EASDSurround :: Eac3AtmosStereoDownmix
pattern EASDSurround = Eac3AtmosStereoDownmix' "SURROUND"

pattern EASDDPL2 :: Eac3AtmosStereoDownmix
pattern EASDDPL2 = Eac3AtmosStereoDownmix' "DPL2"

{-# COMPLETE
  EASDNotIndicated,
  EASDStereo,
  EASDSurround,
  EASDDPL2,
  Eac3AtmosStereoDownmix'
  #-}
