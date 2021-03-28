{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresTelecine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresTelecine
  ( ProresTelecine
    ( ProresTelecine'
    , ProresTelecineNone
    , ProresTelecineHard
    , fromProresTelecine
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
newtype ProresTelecine = ProresTelecine'{fromProresTelecine ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern ProresTelecineNone :: ProresTelecine
pattern ProresTelecineNone = ProresTelecine' "NONE"

pattern ProresTelecineHard :: ProresTelecine
pattern ProresTelecineHard = ProresTelecine' "HARD"

{-# COMPLETE 
  ProresTelecineNone,

  ProresTelecineHard,
  ProresTelecine'
  #-}
