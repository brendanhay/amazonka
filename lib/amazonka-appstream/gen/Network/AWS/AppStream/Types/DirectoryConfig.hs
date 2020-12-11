-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.DirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.DirectoryConfig
  ( DirectoryConfig (..),

    -- * Smart constructor
    mkDirectoryConfig,

    -- * Lenses
    dcCreatedTime,
    dcServiceAccountCredentials,
    dcOrganizationalUnitDistinguishedNames,
    dcDirectoryName,
  )
where

import Network.AWS.AppStream.Types.ServiceAccountCredentials
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
--
-- /See:/ 'mkDirectoryConfig' smart constructor.
data DirectoryConfig = DirectoryConfig'
  { createdTime ::
      Lude.Maybe Lude.Timestamp,
    serviceAccountCredentials ::
      Lude.Maybe ServiceAccountCredentials,
    organizationalUnitDistinguishedNames ::
      Lude.Maybe [Lude.Text],
    directoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryConfig' with the minimum fields required to make a request.
--
-- * 'createdTime' - The time the directory configuration was created.
-- * 'directoryName' - The fully qualified name of the directory (for example, corp.example.com).
-- * 'organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
-- * 'serviceAccountCredentials' - The credentials for the service account used by the fleet or image builder to connect to the directory.
mkDirectoryConfig ::
  -- | 'directoryName'
  Lude.Text ->
  DirectoryConfig
mkDirectoryConfig pDirectoryName_ =
  DirectoryConfig'
    { createdTime = Lude.Nothing,
      serviceAccountCredentials = Lude.Nothing,
      organizationalUnitDistinguishedNames = Lude.Nothing,
      directoryName = pDirectoryName_
    }

-- | The time the directory configuration was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCreatedTime :: Lens.Lens' DirectoryConfig (Lude.Maybe Lude.Timestamp)
dcCreatedTime = Lens.lens (createdTime :: DirectoryConfig -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: DirectoryConfig)
{-# DEPRECATED dcCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcServiceAccountCredentials :: Lens.Lens' DirectoryConfig (Lude.Maybe ServiceAccountCredentials)
dcServiceAccountCredentials = Lens.lens (serviceAccountCredentials :: DirectoryConfig -> Lude.Maybe ServiceAccountCredentials) (\s a -> s {serviceAccountCredentials = a} :: DirectoryConfig)
{-# DEPRECATED dcServiceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead." #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOrganizationalUnitDistinguishedNames :: Lens.Lens' DirectoryConfig (Lude.Maybe [Lude.Text])
dcOrganizationalUnitDistinguishedNames = Lens.lens (organizationalUnitDistinguishedNames :: DirectoryConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationalUnitDistinguishedNames = a} :: DirectoryConfig)
{-# DEPRECATED dcOrganizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead." #-}

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDirectoryName :: Lens.Lens' DirectoryConfig Lude.Text
dcDirectoryName = Lens.lens (directoryName :: DirectoryConfig -> Lude.Text) (\s a -> s {directoryName = a} :: DirectoryConfig)
{-# DEPRECATED dcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Lude.FromJSON DirectoryConfig where
  parseJSON =
    Lude.withObject
      "DirectoryConfig"
      ( \x ->
          DirectoryConfig'
            Lude.<$> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ServiceAccountCredentials")
            Lude.<*> ( x Lude..:? "OrganizationalUnitDistinguishedNames"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..: "DirectoryName")
      )
