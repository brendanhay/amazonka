-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
  ( DirectoryServiceAuthenticationRequest (..),

    -- * Smart constructor
    mkDirectoryServiceAuthenticationRequest,

    -- * Lenses
    dsarDirectoryId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Active Directory to be used for client authentication.
--
-- /See:/ 'mkDirectoryServiceAuthenticationRequest' smart constructor.
newtype DirectoryServiceAuthenticationRequest = DirectoryServiceAuthenticationRequest'
  { directoryId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryServiceAuthenticationRequest' with the minimum fields required to make a request.
--
-- * 'directoryId' - The ID of the Active Directory to be used for authentication.
mkDirectoryServiceAuthenticationRequest ::
  DirectoryServiceAuthenticationRequest
mkDirectoryServiceAuthenticationRequest =
  DirectoryServiceAuthenticationRequest'
    { directoryId =
        Lude.Nothing
    }

-- | The ID of the Active Directory to be used for authentication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarDirectoryId :: Lens.Lens' DirectoryServiceAuthenticationRequest (Lude.Maybe Lude.Text)
dsarDirectoryId = Lens.lens (directoryId :: DirectoryServiceAuthenticationRequest -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DirectoryServiceAuthenticationRequest)
{-# DEPRECATED dsarDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.ToQuery DirectoryServiceAuthenticationRequest where
  toQuery DirectoryServiceAuthenticationRequest' {..} =
    Lude.mconcat ["DirectoryId" Lude.=: directoryId]
