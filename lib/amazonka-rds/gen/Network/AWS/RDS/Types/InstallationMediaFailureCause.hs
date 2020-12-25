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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the cause of an installation media failure. Installation media is used for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'mkInstallationMediaFailureCause' smart constructor.
newtype InstallationMediaFailureCause = InstallationMediaFailureCause'
  { -- | The reason that an installation media import failed.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstallationMediaFailureCause' value with any optional fields omitted.
mkInstallationMediaFailureCause ::
  InstallationMediaFailureCause
mkInstallationMediaFailureCause =
  InstallationMediaFailureCause' {message = Core.Nothing}

-- | The reason that an installation media import failed.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfcMessage :: Lens.Lens' InstallationMediaFailureCause (Core.Maybe Types.String)
imfcMessage = Lens.field @"message"
{-# DEPRECATED imfcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromXML InstallationMediaFailureCause where
  parseXML x =
    InstallationMediaFailureCause' Core.<$> (x Core..@? "Message")
