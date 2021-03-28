{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DirectoryServiceAuthentication
  ( DirectoryServiceAuthentication (..)
  -- * Smart constructor
  , mkDirectoryServiceAuthentication
  -- * Lenses
  , dsaDirectoryId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Active Directory.
--
-- /See:/ 'mkDirectoryServiceAuthentication' smart constructor.
newtype DirectoryServiceAuthentication = DirectoryServiceAuthentication'
  { directoryId :: Core.Maybe Core.Text
    -- ^ The ID of the Active Directory used for authentication.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryServiceAuthentication' value with any optional fields omitted.
mkDirectoryServiceAuthentication
    :: DirectoryServiceAuthentication
mkDirectoryServiceAuthentication
  = DirectoryServiceAuthentication'{directoryId = Core.Nothing}

-- | The ID of the Active Directory used for authentication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaDirectoryId :: Lens.Lens' DirectoryServiceAuthentication (Core.Maybe Core.Text)
dsaDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dsaDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

instance Core.FromXML DirectoryServiceAuthentication where
        parseXML x
          = DirectoryServiceAuthentication' Core.<$>
              (x Core..@? "directoryId")
