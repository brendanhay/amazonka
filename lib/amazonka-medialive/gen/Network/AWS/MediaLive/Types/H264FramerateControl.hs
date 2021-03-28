{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FramerateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264FramerateControl
  ( H264FramerateControl
    ( H264FramerateControl'
    , H264FramerateControlInitializeFromSource
    , H264FramerateControlSpecified
    , fromH264FramerateControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Framerate Control
newtype H264FramerateControl = H264FramerateControl'{fromH264FramerateControl
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern H264FramerateControlInitializeFromSource :: H264FramerateControl
pattern H264FramerateControlInitializeFromSource = H264FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern H264FramerateControlSpecified :: H264FramerateControl
pattern H264FramerateControlSpecified = H264FramerateControl' "SPECIFIED"

{-# COMPLETE 
  H264FramerateControlInitializeFromSource,

  H264FramerateControlSpecified,
  H264FramerateControl'
  #-}
