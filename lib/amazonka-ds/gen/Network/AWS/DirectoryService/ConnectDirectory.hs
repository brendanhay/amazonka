{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ConnectDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AD Connector to connect to an on-premises directory.
--
-- Before you call @ConnectDirectory@ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the @ConnectDirectory@ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.ConnectDirectory
  ( -- * Creating a request
    ConnectDirectory (..),
    mkConnectDirectory,

    -- ** Request lenses
    cdShortName,
    cdDescription,
    cdTags,
    cdName,
    cdPassword,
    cdSize,
    cdConnectSettings,

    -- * Destructuring the response
    ConnectDirectoryResponse (..),
    mkConnectDirectoryResponse,

    -- ** Response lenses
    cdrsDirectoryId,
    cdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'ConnectDirectory' operation.
--
-- /See:/ 'mkConnectDirectory' smart constructor.
data ConnectDirectory = ConnectDirectory'
  { shortName ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    password :: Lude.Sensitive Lude.Text,
    size :: DirectorySize,
    connectSettings :: DirectoryConnectSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectDirectory' with the minimum fields required to make a request.
--
-- * 'connectSettings' - A 'DirectoryConnectSettings' object that contains additional information for the operation.
-- * 'description' - A description for the directory.
-- * 'name' - The fully qualified name of the on-premises directory, such as @corp.example.com@ .
-- * 'password' - The password for the on-premises user account.
-- * 'shortName' - The NetBIOS name of the on-premises directory, such as @CORP@ .
-- * 'size' - The size of the directory.
-- * 'tags' - The tags to be assigned to AD Connector.
mkConnectDirectory ::
  -- | 'name'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  -- | 'size'
  DirectorySize ->
  -- | 'connectSettings'
  DirectoryConnectSettings ->
  ConnectDirectory
mkConnectDirectory pName_ pPassword_ pSize_ pConnectSettings_ =
  ConnectDirectory'
    { shortName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      password = pPassword_,
      size = pSize_,
      connectSettings = pConnectSettings_
    }

-- | The NetBIOS name of the on-premises directory, such as @CORP@ .
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdShortName :: Lens.Lens' ConnectDirectory (Lude.Maybe Lude.Text)
cdShortName = Lens.lens (shortName :: ConnectDirectory -> Lude.Maybe Lude.Text) (\s a -> s {shortName = a} :: ConnectDirectory)
{-# DEPRECATED cdShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | A description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDescription :: Lens.Lens' ConnectDirectory (Lude.Maybe Lude.Text)
cdDescription = Lens.lens (description :: ConnectDirectory -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConnectDirectory)
{-# DEPRECATED cdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to be assigned to AD Connector.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' ConnectDirectory (Lude.Maybe [Tag])
cdTags = Lens.lens (tags :: ConnectDirectory -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ConnectDirectory)
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The fully qualified name of the on-premises directory, such as @corp.example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' ConnectDirectory Lude.Text
cdName = Lens.lens (name :: ConnectDirectory -> Lude.Text) (\s a -> s {name = a} :: ConnectDirectory)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The password for the on-premises user account.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPassword :: Lens.Lens' ConnectDirectory (Lude.Sensitive Lude.Text)
cdPassword = Lens.lens (password :: ConnectDirectory -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: ConnectDirectory)
{-# DEPRECATED cdPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The size of the directory.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSize :: Lens.Lens' ConnectDirectory DirectorySize
cdSize = Lens.lens (size :: ConnectDirectory -> DirectorySize) (\s a -> s {size = a} :: ConnectDirectory)
{-# DEPRECATED cdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | A 'DirectoryConnectSettings' object that contains additional information for the operation.
--
-- /Note:/ Consider using 'connectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConnectSettings :: Lens.Lens' ConnectDirectory DirectoryConnectSettings
cdConnectSettings = Lens.lens (connectSettings :: ConnectDirectory -> DirectoryConnectSettings) (\s a -> s {connectSettings = a} :: ConnectDirectory)
{-# DEPRECATED cdConnectSettings "Use generic-lens or generic-optics with 'connectSettings' instead." #-}

instance Lude.AWSRequest ConnectDirectory where
  type Rs ConnectDirectory = ConnectDirectoryResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConnectDirectoryResponse'
            Lude.<$> (x Lude..?> "DirectoryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConnectDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.ConnectDirectory" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConnectDirectory where
  toJSON ConnectDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShortName" Lude..=) Lude.<$> shortName,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Password" Lude..= password),
            Lude.Just ("Size" Lude..= size),
            Lude.Just ("ConnectSettings" Lude..= connectSettings)
          ]
      )

instance Lude.ToPath ConnectDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery ConnectDirectory where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'ConnectDirectory' operation.
--
-- /See:/ 'mkConnectDirectoryResponse' smart constructor.
data ConnectDirectoryResponse = ConnectDirectoryResponse'
  { directoryId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the new directory.
-- * 'responseStatus' - The response status code.
mkConnectDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConnectDirectoryResponse
mkConnectDirectoryResponse pResponseStatus_ =
  ConnectDirectoryResponse'
    { directoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the new directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDirectoryId :: Lens.Lens' ConnectDirectoryResponse (Lude.Maybe Lude.Text)
cdrsDirectoryId = Lens.lens (directoryId :: ConnectDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: ConnectDirectoryResponse)
{-# DEPRECATED cdrsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' ConnectDirectoryResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: ConnectDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConnectDirectoryResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
