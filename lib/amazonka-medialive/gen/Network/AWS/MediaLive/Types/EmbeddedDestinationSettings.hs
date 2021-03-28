{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
  ( EmbeddedDestinationSettings (..)
  -- * Smart constructor
  , mkEmbeddedDestinationSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Embedded Destination Settings
--
-- /See:/ 'mkEmbeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmbeddedDestinationSettings' value with any optional fields omitted.
mkEmbeddedDestinationSettings
    :: EmbeddedDestinationSettings
mkEmbeddedDestinationSettings = EmbeddedDestinationSettings'

instance Core.FromJSON EmbeddedDestinationSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON EmbeddedDestinationSettings where
        parseJSON
          = Core.withObject "EmbeddedDestinationSettings" Core.$
              \ x -> Core.pure EmbeddedDestinationSettings'
