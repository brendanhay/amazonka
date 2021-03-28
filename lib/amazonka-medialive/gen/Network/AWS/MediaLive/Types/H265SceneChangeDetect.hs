{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
    ( H265SceneChangeDetect'
    , H265SceneChangeDetectDisabled
    , H265SceneChangeDetectEnabled
    , fromH265SceneChangeDetect
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H265 Scene Change Detect
newtype H265SceneChangeDetect = H265SceneChangeDetect'{fromH265SceneChangeDetect
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern H265SceneChangeDetectDisabled :: H265SceneChangeDetect
pattern H265SceneChangeDetectDisabled = H265SceneChangeDetect' "DISABLED"

pattern H265SceneChangeDetectEnabled :: H265SceneChangeDetect
pattern H265SceneChangeDetectEnabled = H265SceneChangeDetect' "ENABLED"

{-# COMPLETE 
  H265SceneChangeDetectDisabled,

  H265SceneChangeDetectEnabled,
  H265SceneChangeDetect'
  #-}
