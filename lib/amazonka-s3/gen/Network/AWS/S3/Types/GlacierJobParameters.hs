{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.GlacierJobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.GlacierJobParameters
  ( GlacierJobParameters (..)
  -- * Smart constructor
  , mkGlacierJobParameters
  -- * Lenses
  , gjpTier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Tier as Types

-- | Container for S3 Glacier job parameters.
--
-- /See:/ 'mkGlacierJobParameters' smart constructor.
newtype GlacierJobParameters = GlacierJobParameters'
  { tier :: Types.Tier
    -- ^ Retrieval tier at which the restore will be processed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GlacierJobParameters' value with any optional fields omitted.
mkGlacierJobParameters
    :: Types.Tier -- ^ 'tier'
    -> GlacierJobParameters
mkGlacierJobParameters tier = GlacierJobParameters'{tier}

-- | Retrieval tier at which the restore will be processed.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjpTier :: Lens.Lens' GlacierJobParameters Types.Tier
gjpTier = Lens.field @"tier"
{-# INLINEABLE gjpTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

instance Core.ToXML GlacierJobParameters where
        toXML GlacierJobParameters{..} = Core.toXMLElement "Tier" tier
