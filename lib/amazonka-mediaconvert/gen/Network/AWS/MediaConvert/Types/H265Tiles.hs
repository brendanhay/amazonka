{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Tiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265Tiles
  ( H265Tiles
    ( H265Tiles'
    , H265TilesDisabled
    , H265TilesEnabled
    , fromH265Tiles
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
newtype H265Tiles = H265Tiles'{fromH265Tiles :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern H265TilesDisabled :: H265Tiles
pattern H265TilesDisabled = H265Tiles' "DISABLED"

pattern H265TilesEnabled :: H265Tiles
pattern H265TilesEnabled = H265Tiles' "ENABLED"

{-# COMPLETE 
  H265TilesDisabled,

  H265TilesEnabled,
  H265Tiles'
  #-}
