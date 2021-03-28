{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
  ( EmbeddedPlusScte20DestinationSettings (..)
  -- * Smart constructor
  , mkEmbeddedPlusScte20DestinationSettings
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Embedded Plus Scte20 Destination Settings
--
-- /See:/ 'mkEmbeddedPlusScte20DestinationSettings' smart constructor.
data EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmbeddedPlusScte20DestinationSettings' value with any optional fields omitted.
mkEmbeddedPlusScte20DestinationSettings
    :: EmbeddedPlusScte20DestinationSettings
mkEmbeddedPlusScte20DestinationSettings
  = EmbeddedPlusScte20DestinationSettings'

instance Core.FromJSON EmbeddedPlusScte20DestinationSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.FromJSON EmbeddedPlusScte20DestinationSettings where
        parseJSON
          = Core.withObject "EmbeddedPlusScte20DestinationSettings" Core.$
              \ x -> Core.pure EmbeddedPlusScte20DestinationSettings'
