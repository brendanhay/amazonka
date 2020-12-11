-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
  ( Scte20PlusEmbeddedDestinationSettings (..),

    -- * Smart constructor
    mkScte20PlusEmbeddedDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Scte20 Plus Embedded Destination Settings
--
-- /See:/ 'mkScte20PlusEmbeddedDestinationSettings' smart constructor.
data Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte20PlusEmbeddedDestinationSettings' with the minimum fields required to make a request.
mkScte20PlusEmbeddedDestinationSettings ::
  Scte20PlusEmbeddedDestinationSettings
mkScte20PlusEmbeddedDestinationSettings =
  Scte20PlusEmbeddedDestinationSettings'

instance Lude.FromJSON Scte20PlusEmbeddedDestinationSettings where
  parseJSON =
    Lude.withObject
      "Scte20PlusEmbeddedDestinationSettings"
      (\x -> Lude.pure Scte20PlusEmbeddedDestinationSettings')

instance Lude.ToJSON Scte20PlusEmbeddedDestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
