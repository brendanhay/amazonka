{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Mpeg2ScanType
  ( Mpeg2ScanType
    ( Mpeg2ScanType'
    , Mpeg2ScanTypeInterlaced
    , Mpeg2ScanTypeProgressive
    , fromMpeg2ScanType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Mpeg2 Scan Type
newtype Mpeg2ScanType = Mpeg2ScanType'{fromMpeg2ScanType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern Mpeg2ScanTypeInterlaced :: Mpeg2ScanType
pattern Mpeg2ScanTypeInterlaced = Mpeg2ScanType' "INTERLACED"

pattern Mpeg2ScanTypeProgressive :: Mpeg2ScanType
pattern Mpeg2ScanTypeProgressive = Mpeg2ScanType' "PROGRESSIVE"

{-# COMPLETE 
  Mpeg2ScanTypeInterlaced,

  Mpeg2ScanTypeProgressive,
  Mpeg2ScanType'
  #-}
