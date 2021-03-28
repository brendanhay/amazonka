{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Vc3InterlaceMode
  ( Vc3InterlaceMode
    ( Vc3InterlaceMode'
    , Vc3InterlaceModeInterlaced
    , Vc3InterlaceModeProgressive
    , fromVc3InterlaceMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
newtype Vc3InterlaceMode = Vc3InterlaceMode'{fromVc3InterlaceMode
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern Vc3InterlaceModeInterlaced :: Vc3InterlaceMode
pattern Vc3InterlaceModeInterlaced = Vc3InterlaceMode' "INTERLACED"

pattern Vc3InterlaceModeProgressive :: Vc3InterlaceMode
pattern Vc3InterlaceModeProgressive = Vc3InterlaceMode' "PROGRESSIVE"

{-# COMPLETE 
  Vc3InterlaceModeInterlaced,

  Vc3InterlaceModeProgressive,
  Vc3InterlaceMode'
  #-}
