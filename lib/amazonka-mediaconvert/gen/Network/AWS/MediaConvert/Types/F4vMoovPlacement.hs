{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vMoovPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.F4vMoovPlacement
  ( F4vMoovPlacement
    ( F4vMoovPlacement'
    , F4vMoovPlacementProgressiveDownload
    , F4vMoovPlacementNormal
    , fromF4vMoovPlacement
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement'{fromF4vMoovPlacement
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern F4vMoovPlacementProgressiveDownload :: F4vMoovPlacement
pattern F4vMoovPlacementProgressiveDownload = F4vMoovPlacement' "PROGRESSIVE_DOWNLOAD"

pattern F4vMoovPlacementNormal :: F4vMoovPlacement
pattern F4vMoovPlacementNormal = F4vMoovPlacement' "NORMAL"

{-# COMPLETE 
  F4vMoovPlacementProgressiveDownload,

  F4vMoovPlacementNormal,
  F4vMoovPlacement'
  #-}
