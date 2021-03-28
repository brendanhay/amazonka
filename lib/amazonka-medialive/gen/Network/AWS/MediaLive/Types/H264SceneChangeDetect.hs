{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264SceneChangeDetect
  ( H264SceneChangeDetect
    ( H264SceneChangeDetect'
    , H264SceneChangeDetectDisabled
    , H264SceneChangeDetectEnabled
    , fromH264SceneChangeDetect
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Scene Change Detect
newtype H264SceneChangeDetect = H264SceneChangeDetect'{fromH264SceneChangeDetect
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern H264SceneChangeDetectDisabled :: H264SceneChangeDetect
pattern H264SceneChangeDetectDisabled = H264SceneChangeDetect' "DISABLED"

pattern H264SceneChangeDetectEnabled :: H264SceneChangeDetect
pattern H264SceneChangeDetectEnabled = H264SceneChangeDetect' "ENABLED"

{-# COMPLETE 
  H264SceneChangeDetectDisabled,

  H264SceneChangeDetectEnabled,
  H264SceneChangeDetect'
  #-}
