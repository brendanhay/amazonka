{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Scte20 Plus Embedded Destination Settings
--
-- /See:/ 'mkScte20PlusEmbeddedDestinationSettings' smart constructor.
data Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte20PlusEmbeddedDestinationSettings' value with any optional fields omitted.
mkScte20PlusEmbeddedDestinationSettings ::
  Scte20PlusEmbeddedDestinationSettings
mkScte20PlusEmbeddedDestinationSettings =
  Scte20PlusEmbeddedDestinationSettings'

instance Core.FromJSON Scte20PlusEmbeddedDestinationSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.FromJSON Scte20PlusEmbeddedDestinationSettings where
  parseJSON =
    Core.withObject "Scte20PlusEmbeddedDestinationSettings" Core.$
      \x -> Core.pure Scte20PlusEmbeddedDestinationSettings'
