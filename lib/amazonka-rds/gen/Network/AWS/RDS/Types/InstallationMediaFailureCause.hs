{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.InstallationMediaFailureCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMediaFailureCause
  ( InstallationMediaFailureCause (..),

    -- * Smart constructor
    mkInstallationMediaFailureCause,

    -- * Lenses
    imfcMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the cause of an installation media failure. Installation media is used for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'mkInstallationMediaFailureCause' smart constructor.
newtype InstallationMediaFailureCause = InstallationMediaFailureCause'
  { message ::
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

-- | Creates a value of 'InstallationMediaFailureCause' with the minimum fields required to make a request.
--
-- * 'message' - The reason that an installation media import failed.
mkInstallationMediaFailureCause ::
  InstallationMediaFailureCause
mkInstallationMediaFailureCause =
  InstallationMediaFailureCause' {message = Lude.Nothing}

-- | The reason that an installation media import failed.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfcMessage :: Lens.Lens' InstallationMediaFailureCause (Lude.Maybe Lude.Text)
imfcMessage = Lens.lens (message :: InstallationMediaFailureCause -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InstallationMediaFailureCause)
{-# DEPRECATED imfcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML InstallationMediaFailureCause where
  parseXML x =
    InstallationMediaFailureCause' Lude.<$> (x Lude..@? "Message")
