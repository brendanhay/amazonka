{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsIngest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.HlsIngest
  ( HlsIngest (..)
  -- * Smart constructor
  , mkHlsIngest
  -- * Lenses
  , hiIngestEndpoints
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.IngestEndpoint as Types
import qualified Network.AWS.Prelude as Core

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'mkHlsIngest' smart constructor.
newtype HlsIngest = HlsIngest'
  { ingestEndpoints :: Core.Maybe [Types.IngestEndpoint]
    -- ^ A list of endpoints to which the source stream should be sent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HlsIngest' value with any optional fields omitted.
mkHlsIngest
    :: HlsIngest
mkHlsIngest = HlsIngest'{ingestEndpoints = Core.Nothing}

-- | A list of endpoints to which the source stream should be sent.
--
-- /Note:/ Consider using 'ingestEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiIngestEndpoints :: Lens.Lens' HlsIngest (Core.Maybe [Types.IngestEndpoint])
hiIngestEndpoints = Lens.field @"ingestEndpoints"
{-# INLINEABLE hiIngestEndpoints #-}
{-# DEPRECATED ingestEndpoints "Use generic-lens or generic-optics with 'ingestEndpoints' instead"  #-}

instance Core.FromJSON HlsIngest where
        parseJSON
          = Core.withObject "HlsIngest" Core.$
              \ x -> HlsIngest' Core.<$> (x Core..:? "ingestEndpoints")
