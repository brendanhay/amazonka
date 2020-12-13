{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Directory Config object in AppStream 2.0. This object includes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
module Network.AWS.AppStream.UpdateDirectoryConfig
  ( -- * Creating a request
    UpdateDirectoryConfig (..),
    mkUpdateDirectoryConfig,

    -- ** Request lenses
    udcServiceAccountCredentials,
    udcOrganizationalUnitDistinguishedNames,
    udcDirectoryName,

    -- * Destructuring the response
    UpdateDirectoryConfigResponse (..),
    mkUpdateDirectoryConfigResponse,

    -- ** Response lenses
    udcrsDirectoryConfig,
    udcrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDirectoryConfig' smart constructor.
data UpdateDirectoryConfig = UpdateDirectoryConfig'
  { -- | The credentials for the service account used by the fleet or image builder to connect to the directory.
    serviceAccountCredentials :: Lude.Maybe ServiceAccountCredentials,
    -- | The distinguished names of the organizational units for computer accounts.
    organizationalUnitDistinguishedNames :: Lude.Maybe [Lude.Text],
    -- | The name of the Directory Config object.
    directoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDirectoryConfig' with the minimum fields required to make a request.
--
-- * 'serviceAccountCredentials' - The credentials for the service account used by the fleet or image builder to connect to the directory.
-- * 'organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
-- * 'directoryName' - The name of the Directory Config object.
mkUpdateDirectoryConfig ::
  -- | 'directoryName'
  Lude.Text ->
  UpdateDirectoryConfig
mkUpdateDirectoryConfig pDirectoryName_ =
  UpdateDirectoryConfig'
    { serviceAccountCredentials = Lude.Nothing,
      organizationalUnitDistinguishedNames = Lude.Nothing,
      directoryName = pDirectoryName_
    }

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcServiceAccountCredentials :: Lens.Lens' UpdateDirectoryConfig (Lude.Maybe ServiceAccountCredentials)
udcServiceAccountCredentials = Lens.lens (serviceAccountCredentials :: UpdateDirectoryConfig -> Lude.Maybe ServiceAccountCredentials) (\s a -> s {serviceAccountCredentials = a} :: UpdateDirectoryConfig)
{-# DEPRECATED udcServiceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead." #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcOrganizationalUnitDistinguishedNames :: Lens.Lens' UpdateDirectoryConfig (Lude.Maybe [Lude.Text])
udcOrganizationalUnitDistinguishedNames = Lens.lens (organizationalUnitDistinguishedNames :: UpdateDirectoryConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationalUnitDistinguishedNames = a} :: UpdateDirectoryConfig)
{-# DEPRECATED udcOrganizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead." #-}

-- | The name of the Directory Config object.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDirectoryName :: Lens.Lens' UpdateDirectoryConfig Lude.Text
udcDirectoryName = Lens.lens (directoryName :: UpdateDirectoryConfig -> Lude.Text) (\s a -> s {directoryName = a} :: UpdateDirectoryConfig)
{-# DEPRECATED udcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Lude.AWSRequest UpdateDirectoryConfig where
  type Rs UpdateDirectoryConfig = UpdateDirectoryConfigResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDirectoryConfigResponse'
            Lude.<$> (x Lude..?> "DirectoryConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDirectoryConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.UpdateDirectoryConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDirectoryConfig where
  toJSON UpdateDirectoryConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceAccountCredentials" Lude..=)
              Lude.<$> serviceAccountCredentials,
            ("OrganizationalUnitDistinguishedNames" Lude..=)
              Lude.<$> organizationalUnitDistinguishedNames,
            Lude.Just ("DirectoryName" Lude..= directoryName)
          ]
      )

instance Lude.ToPath UpdateDirectoryConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDirectoryConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDirectoryConfigResponse' smart constructor.
data UpdateDirectoryConfigResponse = UpdateDirectoryConfigResponse'
  { -- | Information about the Directory Config object.
    directoryConfig :: Lude.Maybe DirectoryConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- * 'directoryConfig' - Information about the Directory Config object.
-- * 'responseStatus' - The response status code.
mkUpdateDirectoryConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDirectoryConfigResponse
mkUpdateDirectoryConfigResponse pResponseStatus_ =
  UpdateDirectoryConfigResponse'
    { directoryConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Directory Config object.
--
-- /Note:/ Consider using 'directoryConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsDirectoryConfig :: Lens.Lens' UpdateDirectoryConfigResponse (Lude.Maybe DirectoryConfig)
udcrsDirectoryConfig = Lens.lens (directoryConfig :: UpdateDirectoryConfigResponse -> Lude.Maybe DirectoryConfig) (\s a -> s {directoryConfig = a} :: UpdateDirectoryConfigResponse)
{-# DEPRECATED udcrsDirectoryConfig "Use generic-lens or generic-optics with 'directoryConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsResponseStatus :: Lens.Lens' UpdateDirectoryConfigResponse Lude.Int
udcrsResponseStatus = Lens.lens (responseStatus :: UpdateDirectoryConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDirectoryConfigResponse)
{-# DEPRECATED udcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
