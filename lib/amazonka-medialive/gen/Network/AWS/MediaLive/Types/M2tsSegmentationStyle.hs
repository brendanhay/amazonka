{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsSegmentationStyle
  ( M2tsSegmentationStyle
    ( M2tsSegmentationStyle'
    , M2tsSegmentationStyleMaintainCadence
    , M2tsSegmentationStyleResetCadence
    , fromM2tsSegmentationStyle
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Segmentation Style
newtype M2tsSegmentationStyle = M2tsSegmentationStyle'{fromM2tsSegmentationStyle
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern M2tsSegmentationStyleMaintainCadence :: M2tsSegmentationStyle
pattern M2tsSegmentationStyleMaintainCadence = M2tsSegmentationStyle' "MAINTAIN_CADENCE"

pattern M2tsSegmentationStyleResetCadence :: M2tsSegmentationStyle
pattern M2tsSegmentationStyleResetCadence = M2tsSegmentationStyle' "RESET_CADENCE"

{-# COMPLETE 
  M2tsSegmentationStyleMaintainCadence,

  M2tsSegmentationStyleResetCadence,
  M2tsSegmentationStyle'
  #-}
