{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
  ( RtmpCaptionInfoDestinationSettings (..)
  -- * Smart constructor
  , mkRtmpCaptionInfoDestinationSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'mkRtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RtmpCaptionInfoDestinationSettings' value with any optional fields omitted.
mkRtmpCaptionInfoDestinationSettings
    :: RtmpCaptionInfoDestinationSettings
mkRtmpCaptionInfoDestinationSettings
  = RtmpCaptionInfoDestinationSettings'

instance Core.FromJSON RtmpCaptionInfoDestinationSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON RtmpCaptionInfoDestinationSettings where
        parseJSON
          = Core.withObject "RtmpCaptionInfoDestinationSettings" Core.$
              \ x -> Core.pure RtmpCaptionInfoDestinationSettings'
