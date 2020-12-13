{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateDirectoryConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Directory Config object in AppStream 2.0. This object includes the configuration information required to join fleets and image builders to Microsoft Active Directory domains.
module Network.AWS.AppStream.CreateDirectoryConfig
  ( -- * Creating a request
    CreateDirectoryConfig (..),
    mkCreateDirectoryConfig,

    -- ** Request lenses
    cdcServiceAccountCredentials,
    cdcOrganizationalUnitDistinguishedNames,
    cdcDirectoryName,

    -- * Destructuring the response
    CreateDirectoryConfigResponse (..),
    mkCreateDirectoryConfigResponse,

    -- ** Response lenses
    cdcrsDirectoryConfig,
    cdcrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDirectoryConfig' smart constructor.
data CreateDirectoryConfig = CreateDirectoryConfig'
  { -- | The credentials for the service account used by the fleet or image builder to connect to the directory.
    serviceAccountCredentials :: Lude.Maybe ServiceAccountCredentials,
    -- | The distinguished names of the organizational units for computer accounts.
    organizationalUnitDistinguishedNames :: [Lude.Text],
    -- | The fully qualified name of the directory (for example, corp.example.com).
    directoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectoryConfig' with the minimum fields required to make a request.
--
-- * 'serviceAccountCredentials' - The credentials for the service account used by the fleet or image builder to connect to the directory.
-- * 'organizationalUnitDistinguishedNames' - The distinguished names of the organizational units for computer accounts.
-- * 'directoryName' - The fully qualified name of the directory (for example, corp.example.com).
mkCreateDirectoryConfig ::
  -- | 'directoryName'
  Lude.Text ->
  CreateDirectoryConfig
mkCreateDirectoryConfig pDirectoryName_ =
  CreateDirectoryConfig'
    { serviceAccountCredentials = Lude.Nothing,
      organizationalUnitDistinguishedNames = Lude.mempty,
      directoryName = pDirectoryName_
    }

-- | The credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /Note:/ Consider using 'serviceAccountCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcServiceAccountCredentials :: Lens.Lens' CreateDirectoryConfig (Lude.Maybe ServiceAccountCredentials)
cdcServiceAccountCredentials = Lens.lens (serviceAccountCredentials :: CreateDirectoryConfig -> Lude.Maybe ServiceAccountCredentials) (\s a -> s {serviceAccountCredentials = a} :: CreateDirectoryConfig)
{-# DEPRECATED cdcServiceAccountCredentials "Use generic-lens or generic-optics with 'serviceAccountCredentials' instead." #-}

-- | The distinguished names of the organizational units for computer accounts.
--
-- /Note:/ Consider using 'organizationalUnitDistinguishedNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcOrganizationalUnitDistinguishedNames :: Lens.Lens' CreateDirectoryConfig [Lude.Text]
cdcOrganizationalUnitDistinguishedNames = Lens.lens (organizationalUnitDistinguishedNames :: CreateDirectoryConfig -> [Lude.Text]) (\s a -> s {organizationalUnitDistinguishedNames = a} :: CreateDirectoryConfig)
{-# DEPRECATED cdcOrganizationalUnitDistinguishedNames "Use generic-lens or generic-optics with 'organizationalUnitDistinguishedNames' instead." #-}

-- | The fully qualified name of the directory (for example, corp.example.com).
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDirectoryName :: Lens.Lens' CreateDirectoryConfig Lude.Text
cdcDirectoryName = Lens.lens (directoryName :: CreateDirectoryConfig -> Lude.Text) (\s a -> s {directoryName = a} :: CreateDirectoryConfig)
{-# DEPRECATED cdcDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

instance Lude.AWSRequest CreateDirectoryConfig where
  type Rs CreateDirectoryConfig = CreateDirectoryConfigResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDirectoryConfigResponse'
            Lude.<$> (x Lude..?> "DirectoryConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDirectoryConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.CreateDirectoryConfig" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDirectoryConfig where
  toJSON CreateDirectoryConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServiceAccountCredentials" Lude..=)
              Lude.<$> serviceAccountCredentials,
            Lude.Just
              ( "OrganizationalUnitDistinguishedNames"
                  Lude..= organizationalUnitDistinguishedNames
              ),
            Lude.Just ("DirectoryName" Lude..= directoryName)
          ]
      )

instance Lude.ToPath CreateDirectoryConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDirectoryConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDirectoryConfigResponse' smart constructor.
data CreateDirectoryConfigResponse = CreateDirectoryConfigResponse'
  { -- | Information about the directory configuration.
    directoryConfig :: Lude.Maybe DirectoryConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectoryConfigResponse' with the minimum fields required to make a request.
--
-- * 'directoryConfig' - Information about the directory configuration.
-- * 'responseStatus' - The response status code.
mkCreateDirectoryConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDirectoryConfigResponse
mkCreateDirectoryConfigResponse pResponseStatus_ =
  CreateDirectoryConfigResponse'
    { directoryConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the directory configuration.
--
-- /Note:/ Consider using 'directoryConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsDirectoryConfig :: Lens.Lens' CreateDirectoryConfigResponse (Lude.Maybe DirectoryConfig)
cdcrsDirectoryConfig = Lens.lens (directoryConfig :: CreateDirectoryConfigResponse -> Lude.Maybe DirectoryConfig) (\s a -> s {directoryConfig = a} :: CreateDirectoryConfigResponse)
{-# DEPRECATED cdcrsDirectoryConfig "Use generic-lens or generic-optics with 'directoryConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDirectoryConfigResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDirectoryConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDirectoryConfigResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
