{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.DomainJoinInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DomainJoinInfo
  ( DomainJoinInfo (..),

    -- * Smart constructor
    mkDomainJoinInfo,

    -- * Lenses
    djiDirectoryName,
    djiOrganizationalUnitDistinguishedName,
  )
where

import qualified Network.AWS.AppStream.Types.DirectoryName as Types
import qualified Network.AWS.AppStream.Types.OrganizationalUnitDistinguishedName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
-- /See:/ 'mkDomainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { -- | The fully qualified name of the directory (for example, corp.example.com).
    directoryName :: Core.Maybe Types.DirectoryName,
    -- | The distinguished name of the organizational unit for computer accounts.
    organizationalUnitDistinguishedName :: Core.Maybe Types.OrganizationalUnitDistinguishedName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainJoinInfo' value with any optional fields omitted.
mkDomainJoinInfo ::
  DomainJoinInfo
mkDomainJoinInfo =
  DomainJoinInfo'
    { directoryName = Core.Nothing,
      organizationalUnitDistinguishedName = Core.Nothing
    }

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djiDirectoryName :: Lens.Lens' DomainJoinInfo (Core.Maybe Types.DirectoryName)
djiDirectoryName = Lens.field @"directoryName"
{-# DEPRECATED djiDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

-- | The distinguished name of the organizational unit for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djiOrganizationalUnitDistinguishedName :: Lens.Lens' DomainJoinInfo (Core.Maybe Types.OrganizationalUnitDistinguishedName)
djiOrganizationalUnitDistinguishedName = Lens.field @"organizationalUnitDistinguishedName"
{-# DEPRECATED djiOrganizationalUnitDistinguishedName "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedName' instead." #-}

instance Core.FromJSON DomainJoinInfo where
  toJSON DomainJoinInfo {..} =
    Core.object
      ( Core.catMaybes
          [ ("DirectoryName" Core..=) Core.<$> directoryName,
            ("OrganizationalUnitDistinguishedName" Core..=)
              Core.<$> organizationalUnitDistinguishedName
          ]
      )

instance Core.FromJSON DomainJoinInfo where
  parseJSON =
    Core.withObject "DomainJoinInfo" Core.$
      \x ->
        DomainJoinInfo'
          Core.<$> (x Core..:? "DirectoryName")
          Core.<*> (x Core..:? "OrganizationalUnitDistinguishedName")
