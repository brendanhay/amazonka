-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WebvttDestinationSettings
  ( WebvttDestinationSettings (..),

    -- * Smart constructor
    mkWebvttDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Webvtt Destination Settings
--
-- /See:/ 'mkWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebvttDestinationSettings' with the minimum fields required to make a request.
mkWebvttDestinationSettings ::
  WebvttDestinationSettings
mkWebvttDestinationSettings = WebvttDestinationSettings'

instance Lude.FromJSON WebvttDestinationSettings where
  parseJSON =
    Lude.withObject
      "WebvttDestinationSettings"
      (\x -> Lude.pure WebvttDestinationSettings')

instance Lude.ToJSON WebvttDestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
