{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Active Directory to be used for client authentication.
--
-- /See:/ 'mkDirectoryServiceAuthenticationRequest' smart constructor.
newtype DirectoryServiceAuthenticationRequest = DirectoryServiceAuthenticationRequest'
  { -- | The ID of the Active Directory to be used for authentication.
    directoryId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryServiceAuthenticationRequest' value with any optional fields omitted.
mkDirectoryServiceAuthenticationRequest ::
  DirectoryServiceAuthenticationRequest
mkDirectoryServiceAuthenticationRequest =
  DirectoryServiceAuthenticationRequest'
    { directoryId =
        Core.Nothing
    }

-- | The ID of the Active Directory to be used for authentication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarDirectoryId :: Lens.Lens' DirectoryServiceAuthenticationRequest (Core.Maybe Types.String)
dsarDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dsarDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}
