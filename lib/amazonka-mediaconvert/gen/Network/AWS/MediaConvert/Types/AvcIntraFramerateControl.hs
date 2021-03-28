{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
  ( AvcIntraFramerateControl
    ( AvcIntraFramerateControl'
    , AvcIntraFramerateControlInitializeFromSource
    , AvcIntraFramerateControlSpecified
    , fromAvcIntraFramerateControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
newtype AvcIntraFramerateControl = AvcIntraFramerateControl'{fromAvcIntraFramerateControl
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern AvcIntraFramerateControlInitializeFromSource :: AvcIntraFramerateControl
pattern AvcIntraFramerateControlInitializeFromSource = AvcIntraFramerateControl' "INITIALIZE_FROM_SOURCE"

pattern AvcIntraFramerateControlSpecified :: AvcIntraFramerateControl
pattern AvcIntraFramerateControlSpecified = AvcIntraFramerateControl' "SPECIFIED"

{-# COMPLETE 
  AvcIntraFramerateControlInitializeFromSource,

  AvcIntraFramerateControlSpecified,
  AvcIntraFramerateControl'
  #-}
