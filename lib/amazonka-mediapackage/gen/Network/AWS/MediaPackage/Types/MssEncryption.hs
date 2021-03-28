{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.MssEncryption
  ( MssEncryption (..)
  -- * Smart constructor
  , mkMssEncryption
  -- * Lenses
  , meSpekeKeyProvider
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.SpekeKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'mkMssEncryption' smart constructor.
newtype MssEncryption = MssEncryption'
  { spekeKeyProvider :: Types.SpekeKeyProvider
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MssEncryption' value with any optional fields omitted.
mkMssEncryption
    :: Types.SpekeKeyProvider -- ^ 'spekeKeyProvider'
    -> MssEncryption
mkMssEncryption spekeKeyProvider = MssEncryption'{spekeKeyProvider}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSpekeKeyProvider :: Lens.Lens' MssEncryption Types.SpekeKeyProvider
meSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# INLINEABLE meSpekeKeyProvider #-}
{-# DEPRECATED spekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead"  #-}

instance Core.FromJSON MssEncryption where
        toJSON MssEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("spekeKeyProvider" Core..= spekeKeyProvider)])

instance Core.FromJSON MssEncryption where
        parseJSON
          = Core.withObject "MssEncryption" Core.$
              \ x -> MssEncryption' Core.<$> (x Core..: "spekeKeyProvider")
