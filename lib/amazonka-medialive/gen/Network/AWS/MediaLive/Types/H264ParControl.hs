{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ParControl
  ( H264ParControl
    ( H264ParControl'
    , H264ParControlInitializeFromSource
    , H264ParControlSpecified
    , fromH264ParControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Par Control
newtype H264ParControl = H264ParControl'{fromH264ParControl ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern H264ParControlInitializeFromSource :: H264ParControl
pattern H264ParControlInitializeFromSource = H264ParControl' "INITIALIZE_FROM_SOURCE"

pattern H264ParControlSpecified :: H264ParControl
pattern H264ParControlSpecified = H264ParControl' "SPECIFIED"

{-# COMPLETE 
  H264ParControlInitializeFromSource,

  H264ParControlSpecified,
  H264ParControl'
  #-}
