-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
  ( RtmpCaptionInfoDestinationSettings (..),

    -- * Smart constructor
    mkRtmpCaptionInfoDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Rtmp Caption Info Destination Settings
--
-- /See:/ 'mkRtmpCaptionInfoDestinationSettings' smart constructor.
data RtmpCaptionInfoDestinationSettings = RtmpCaptionInfoDestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RtmpCaptionInfoDestinationSettings' with the minimum fields required to make a request.
mkRtmpCaptionInfoDestinationSettings ::
  RtmpCaptionInfoDestinationSettings
mkRtmpCaptionInfoDestinationSettings =
  RtmpCaptionInfoDestinationSettings'

instance Lude.FromJSON RtmpCaptionInfoDestinationSettings where
  parseJSON =
    Lude.withObject
      "RtmpCaptionInfoDestinationSettings"
      (\x -> Lude.pure RtmpCaptionInfoDestinationSettings')

instance Lude.ToJSON RtmpCaptionInfoDestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
