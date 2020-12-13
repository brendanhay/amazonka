{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyUsage
  ( KeyUsage (..),

    -- * Smart constructor
    mkKeyUsage,

    -- * Lenses
    kuName,
  )
where

import Network.AWS.CertificateManager.Types.KeyUsageName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.
--
-- /See:/ 'mkKeyUsage' smart constructor.
newtype KeyUsage = KeyUsage'
  { -- | A string value that contains a Key Usage extension name.
    name :: Lude.Maybe KeyUsageName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyUsage' with the minimum fields required to make a request.
--
-- * 'name' - A string value that contains a Key Usage extension name.
mkKeyUsage ::
  KeyUsage
mkKeyUsage = KeyUsage' {name = Lude.Nothing}

-- | A string value that contains a Key Usage extension name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kuName :: Lens.Lens' KeyUsage (Lude.Maybe KeyUsageName)
kuName = Lens.lens (name :: KeyUsage -> Lude.Maybe KeyUsageName) (\s a -> s {name = a} :: KeyUsage)
{-# DEPRECATED kuName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON KeyUsage where
  parseJSON =
    Lude.withObject
      "KeyUsage"
      (\x -> KeyUsage' Lude.<$> (x Lude..:? "Name"))
