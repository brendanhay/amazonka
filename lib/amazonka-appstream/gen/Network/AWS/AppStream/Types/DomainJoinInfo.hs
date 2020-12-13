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
    djiOrganizationalUnitDistinguishedName,
    djiDirectoryName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
-- /See:/ 'mkDomainJoinInfo' smart constructor.
data DomainJoinInfo = DomainJoinInfo'
  { -- | The distinguished name of the organizational unit for computer accounts.
    organizationalUnitDistinguishedName :: Lude.Maybe Lude.Text,
    -- | The fully qualified name of the directory (for example, corp.example.com).
    directoryName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainJoinInfo' with the minimum fields required to make a request.
--
-- * 'organizationalUnitDistinguishedName' - The distinguished name of the organizational unit for computer accounts.
-- * 'directoryName' - The fully qualified name of the directory (for example, corp.example.com).
mkDomainJoinInfo ::
  DomainJoinInfo
mkDomainJoinInfo =
  DomainJoinInfo'
    { organizationalUnitDistinguishedName =
        Lude.Nothing,
      directoryName = Lude.Nothing
    }

-- | The distinguished name of the organizational unit for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djiOrganizationalUnitDistinguishedName :: Lens.Lens' DomainJoinInfo (Lude.Maybe Lude.Text)
djiOrganizationalUnitDistinguishedName = Lens.lens (organizationalUnitDistinguishedName :: DomainJoinInfo -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitDistinguishedName = a} :: DomainJoinInfo)
{-# DEPRECATED djiOrganizationalUnitDistinguishedName "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedName' instead." #-}

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djiDirectoryName :: Lens.Lens' DomainJoinInfo (Lude.Maybe Lude.Text)
djiDirectoryName = Lens.lens (directoryName :: DomainJoinInfo -> Lude.Maybe Lude.Text) (\s a -> s {directoryName = a} :: DomainJoinInfo)
{-# DEPRECATED djiDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Lude.FromJSON DomainJoinInfo where
  parseJSON =
    Lude.withObject
      "DomainJoinInfo"
      ( \x ->
          DomainJoinInfo'
            Lude.<$> (x Lude..:? "OrganizationalUnitDistinguishedName")
            Lude.<*> (x Lude..:? "DirectoryName")
      )

instance Lude.ToJSON DomainJoinInfo where
  toJSON DomainJoinInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationalUnitDistinguishedName" Lude..=)
              Lude.<$> organizationalUnitDistinguishedName,
            ("DirectoryName" Lude..=) Lude.<$> directoryName
          ]
      )
