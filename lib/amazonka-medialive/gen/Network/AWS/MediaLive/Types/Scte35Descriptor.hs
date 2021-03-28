{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35Descriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35Descriptor
  ( Scte35Descriptor (..)
  -- * Smart constructor
  , mkScte35Descriptor
  -- * Lenses
  , sdScte35DescriptorSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35DescriptorSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Holds one set of SCTE-35 Descriptor Settings.
--
-- /See:/ 'mkScte35Descriptor' smart constructor.
newtype Scte35Descriptor = Scte35Descriptor'
  { scte35DescriptorSettings :: Types.Scte35DescriptorSettings
    -- ^ SCTE-35 Descriptor Settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35Descriptor' value with any optional fields omitted.
mkScte35Descriptor
    :: Types.Scte35DescriptorSettings -- ^ 'scte35DescriptorSettings'
    -> Scte35Descriptor
mkScte35Descriptor scte35DescriptorSettings
  = Scte35Descriptor'{scte35DescriptorSettings}

-- | SCTE-35 Descriptor Settings.
--
-- /Note:/ Consider using 'scte35DescriptorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdScte35DescriptorSettings :: Lens.Lens' Scte35Descriptor Types.Scte35DescriptorSettings
sdScte35DescriptorSettings = Lens.field @"scte35DescriptorSettings"
{-# INLINEABLE sdScte35DescriptorSettings #-}
{-# DEPRECATED scte35DescriptorSettings "Use generic-lens or generic-optics with 'scte35DescriptorSettings' instead"  #-}

instance Core.FromJSON Scte35Descriptor where
        toJSON Scte35Descriptor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("scte35DescriptorSettings" Core..= scte35DescriptorSettings)])

instance Core.FromJSON Scte35Descriptor where
        parseJSON
          = Core.withObject "Scte35Descriptor" Core.$
              \ x ->
                Scte35Descriptor' Core.<$> (x Core..: "scte35DescriptorSettings")
