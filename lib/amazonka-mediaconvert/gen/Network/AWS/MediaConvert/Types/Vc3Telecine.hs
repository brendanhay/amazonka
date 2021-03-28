{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Telecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Vc3Telecine
  ( Vc3Telecine
    ( Vc3Telecine'
    , Vc3TelecineNone
    , Vc3TelecineHard
    , fromVc3Telecine
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
newtype Vc3Telecine = Vc3Telecine'{fromVc3Telecine :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern Vc3TelecineNone :: Vc3Telecine
pattern Vc3TelecineNone = Vc3Telecine' "NONE"

pattern Vc3TelecineHard :: Vc3Telecine
pattern Vc3TelecineHard = Vc3Telecine' "HARD"

{-# COMPLETE 
  Vc3TelecineNone,

  Vc3TelecineHard,
  Vc3Telecine'
  #-}
