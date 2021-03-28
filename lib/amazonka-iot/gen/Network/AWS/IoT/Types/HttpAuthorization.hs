{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpAuthorization
  ( HttpAuthorization (..)
  -- * Smart constructor
  , mkHttpAuthorization
  -- * Lenses
  , haSigv4
  ) where

import qualified Network.AWS.IoT.Types.SigV4Authorization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorization method used to send messages.
--
-- /See:/ 'mkHttpAuthorization' smart constructor.
newtype HttpAuthorization = HttpAuthorization'
  { sigv4 :: Core.Maybe Types.SigV4Authorization
    -- ^ Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpAuthorization' value with any optional fields omitted.
mkHttpAuthorization
    :: HttpAuthorization
mkHttpAuthorization = HttpAuthorization'{sigv4 = Core.Nothing}

-- | Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- /Note:/ Consider using 'sigv4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haSigv4 :: Lens.Lens' HttpAuthorization (Core.Maybe Types.SigV4Authorization)
haSigv4 = Lens.field @"sigv4"
{-# INLINEABLE haSigv4 #-}
{-# DEPRECATED sigv4 "Use generic-lens or generic-optics with 'sigv4' instead"  #-}

instance Core.FromJSON HttpAuthorization where
        toJSON HttpAuthorization{..}
          = Core.object (Core.catMaybes [("sigv4" Core..=) Core.<$> sigv4])

instance Core.FromJSON HttpAuthorization where
        parseJSON
          = Core.withObject "HttpAuthorization" Core.$
              \ x -> HttpAuthorization' Core.<$> (x Core..:? "sigv4")
