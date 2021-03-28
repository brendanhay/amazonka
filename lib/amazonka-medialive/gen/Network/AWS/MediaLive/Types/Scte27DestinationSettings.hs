{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte27DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte27DestinationSettings
  ( Scte27DestinationSettings (..)
  -- * Smart constructor
  , mkScte27DestinationSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Scte27 Destination Settings
--
-- /See:/ 'mkScte27DestinationSettings' smart constructor.
data Scte27DestinationSettings = Scte27DestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte27DestinationSettings' value with any optional fields omitted.
mkScte27DestinationSettings
    :: Scte27DestinationSettings
mkScte27DestinationSettings = Scte27DestinationSettings'

instance Core.FromJSON Scte27DestinationSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON Scte27DestinationSettings where
        parseJSON
          = Core.withObject "Scte27DestinationSettings" Core.$
              \ x -> Core.pure Scte27DestinationSettings'
