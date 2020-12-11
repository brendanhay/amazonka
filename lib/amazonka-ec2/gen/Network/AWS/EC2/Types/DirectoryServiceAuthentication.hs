-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthentication
  ( DirectoryServiceAuthentication (..),

    -- * Smart constructor
    mkDirectoryServiceAuthentication,

    -- * Lenses
    dsaDirectoryId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Active Directory.
--
-- /See:/ 'mkDirectoryServiceAuthentication' smart constructor.
newtype DirectoryServiceAuthentication = DirectoryServiceAuthentication'
  { directoryId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryServiceAuthentication' with the minimum fields required to make a request.
--
-- * 'directoryId' - The ID of the Active Directory used for authentication.
mkDirectoryServiceAuthentication ::
  DirectoryServiceAuthentication
mkDirectoryServiceAuthentication =
  DirectoryServiceAuthentication' {directoryId = Lude.Nothing}

-- | The ID of the Active Directory used for authentication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaDirectoryId :: Lens.Lens' DirectoryServiceAuthentication (Lude.Maybe Lude.Text)
dsaDirectoryId = Lens.lens (directoryId :: DirectoryServiceAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DirectoryServiceAuthentication)
{-# DEPRECATED dsaDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.FromXML DirectoryServiceAuthentication where
  parseXML x =
    DirectoryServiceAuthentication'
      Lude.<$> (x Lude..@? "directoryId")
