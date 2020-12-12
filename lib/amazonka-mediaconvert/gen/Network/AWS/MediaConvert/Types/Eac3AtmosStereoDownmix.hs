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
        DPL2,
        NotIndicated,
        Stereo,
        Surround
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

pattern DPL2 :: Eac3AtmosStereoDownmix
pattern DPL2 = Eac3AtmosStereoDownmix' "DPL2"

pattern NotIndicated :: Eac3AtmosStereoDownmix
pattern NotIndicated = Eac3AtmosStereoDownmix' "NOT_INDICATED"

pattern Stereo :: Eac3AtmosStereoDownmix
pattern Stereo = Eac3AtmosStereoDownmix' "STEREO"

pattern Surround :: Eac3AtmosStereoDownmix
pattern Surround = Eac3AtmosStereoDownmix' "SURROUND"

{-# COMPLETE
  DPL2,
  NotIndicated,
  Stereo,
  Surround,
  Eac3AtmosStereoDownmix'
  #-}
