{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cShortName,
    cSize,
    cName,
    cPassword,
    cConnectSettings,
    cDescription,
    cTags,

    -- * Destructuring the response
    ConnectDirectoryResponse (..),
    mkConnectDirectoryResponse,

    -- ** Response lenses
    crsDirectoryId,
    crsResponseStatus,
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
  { -- | The NetBIOS name of the on-premises directory, such as @CORP@ .
    shortName :: Lude.Maybe Lude.Text,
    -- | The size of the directory.
    size :: DirectorySize,
    -- | The fully qualified name of the on-premises directory, such as @corp.example.com@ .
    name :: Lude.Text,
    -- | The password for the on-premises user account.
    password :: Lude.Sensitive Lude.Text,
    -- | A 'DirectoryConnectSettings' object that contains additional information for the operation.
    connectSettings :: DirectoryConnectSettings,
    -- | A description for the directory.
    description :: Lude.Maybe Lude.Text,
    -- | The tags to be assigned to AD Connector.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectDirectory' with the minimum fields required to make a request.
--
-- * 'shortName' - The NetBIOS name of the on-premises directory, such as @CORP@ .
-- * 'size' - The size of the directory.
-- * 'name' - The fully qualified name of the on-premises directory, such as @corp.example.com@ .
-- * 'password' - The password for the on-premises user account.
-- * 'connectSettings' - A 'DirectoryConnectSettings' object that contains additional information for the operation.
-- * 'description' - A description for the directory.
-- * 'tags' - The tags to be assigned to AD Connector.
mkConnectDirectory ::
  -- | 'size'
  DirectorySize ->
  -- | 'name'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  -- | 'connectSettings'
  DirectoryConnectSettings ->
  ConnectDirectory
mkConnectDirectory pSize_ pName_ pPassword_ pConnectSettings_ =
  ConnectDirectory'
    { shortName = Lude.Nothing,
      size = pSize_,
      name = pName_,
      password = pPassword_,
      connectSettings = pConnectSettings_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The NetBIOS name of the on-premises directory, such as @CORP@ .
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cShortName :: Lens.Lens' ConnectDirectory (Lude.Maybe Lude.Text)
cShortName = Lens.lens (shortName :: ConnectDirectory -> Lude.Maybe Lude.Text) (\s a -> s {shortName = a} :: ConnectDirectory)
{-# DEPRECATED cShortName "Use generic-lens or generic-optics with 'shortName' instead." #-}

-- | The size of the directory.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSize :: Lens.Lens' ConnectDirectory DirectorySize
cSize = Lens.lens (size :: ConnectDirectory -> DirectorySize) (\s a -> s {size = a} :: ConnectDirectory)
{-# DEPRECATED cSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The fully qualified name of the on-premises directory, such as @corp.example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' ConnectDirectory Lude.Text
cName = Lens.lens (name :: ConnectDirectory -> Lude.Text) (\s a -> s {name = a} :: ConnectDirectory)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The password for the on-premises user account.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPassword :: Lens.Lens' ConnectDirectory (Lude.Sensitive Lude.Text)
cPassword = Lens.lens (password :: ConnectDirectory -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: ConnectDirectory)
{-# DEPRECATED cPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | A 'DirectoryConnectSettings' object that contains additional information for the operation.
--
-- /Note:/ Consider using 'connectSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectSettings :: Lens.Lens' ConnectDirectory DirectoryConnectSettings
cConnectSettings = Lens.lens (connectSettings :: ConnectDirectory -> DirectoryConnectSettings) (\s a -> s {connectSettings = a} :: ConnectDirectory)
{-# DEPRECATED cConnectSettings "Use generic-lens or generic-optics with 'connectSettings' instead." #-}

-- | A description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' ConnectDirectory (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: ConnectDirectory -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConnectDirectory)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to be assigned to AD Connector.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' ConnectDirectory (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: ConnectDirectory -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ConnectDirectory)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
            Lude.Just ("Size" Lude..= size),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Password" Lude..= password),
            Lude.Just ("ConnectSettings" Lude..= connectSettings),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
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
  { -- | The identifier of the new directory.
    directoryId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
crsDirectoryId :: Lens.Lens' ConnectDirectoryResponse (Lude.Maybe Lude.Text)
crsDirectoryId = Lens.lens (directoryId :: ConnectDirectoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: ConnectDirectoryResponse)
{-# DEPRECATED crsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ConnectDirectoryResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: ConnectDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConnectDirectoryResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
