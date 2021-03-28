{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
  ( Mpeg2SceneChangeDetect
    ( Mpeg2SceneChangeDetect'
    , Mpeg2SceneChangeDetectDisabled
    , Mpeg2SceneChangeDetectEnabled
    , fromMpeg2SceneChangeDetect
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect'{fromMpeg2SceneChangeDetect
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern Mpeg2SceneChangeDetectDisabled :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetectDisabled = Mpeg2SceneChangeDetect' "DISABLED"

pattern Mpeg2SceneChangeDetectEnabled :: Mpeg2SceneChangeDetect
pattern Mpeg2SceneChangeDetectEnabled = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE 
  Mpeg2SceneChangeDetectDisabled,

  Mpeg2SceneChangeDetectEnabled,
  Mpeg2SceneChangeDetect'
  #-}
