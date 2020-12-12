{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WavCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WavCodingMode
  ( WavCodingMode
      ( WavCodingMode',
        WCMCodingMode10,
        WCMCodingMode20,
        WCMCodingMode40,
        WCMCodingMode80
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Wav Coding Mode
newtype WavCodingMode = WavCodingMode' Lude.Text
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

pattern WCMCodingMode10 :: WavCodingMode
pattern WCMCodingMode10 = WavCodingMode' "CODING_MODE_1_0"

pattern WCMCodingMode20 :: WavCodingMode
pattern WCMCodingMode20 = WavCodingMode' "CODING_MODE_2_0"

pattern WCMCodingMode40 :: WavCodingMode
pattern WCMCodingMode40 = WavCodingMode' "CODING_MODE_4_0"

pattern WCMCodingMode80 :: WavCodingMode
pattern WCMCodingMode80 = WavCodingMode' "CODING_MODE_8_0"

{-# COMPLETE
  WCMCodingMode10,
  WCMCodingMode20,
  WCMCodingMode40,
  WCMCodingMode80,
  WavCodingMode'
  #-}
