{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.KeyUsage
  ( KeyUsage (..)
  -- * Smart constructor
  , mkKeyUsage
  -- * Lenses
  , kuName
  ) where

import qualified Network.AWS.CertificateManager.Types.KeyUsageName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.
--
-- /See:/ 'mkKeyUsage' smart constructor.
newtype KeyUsage = KeyUsage'
  { name :: Core.Maybe Types.KeyUsageName
    -- ^ A string value that contains a Key Usage extension name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KeyUsage' value with any optional fields omitted.
mkKeyUsage
    :: KeyUsage
mkKeyUsage = KeyUsage'{name = Core.Nothing}

-- | A string value that contains a Key Usage extension name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kuName :: Lens.Lens' KeyUsage (Core.Maybe Types.KeyUsageName)
kuName = Lens.field @"name"
{-# INLINEABLE kuName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON KeyUsage where
        parseJSON
          = Core.withObject "KeyUsage" Core.$
              \ x -> KeyUsage' Core.<$> (x Core..:? "Name")
