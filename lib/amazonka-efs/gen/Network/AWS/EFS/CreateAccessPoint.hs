{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateAccessPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EFS access point. An access point is an application-specific view into an EFS file system that applies an operating system user and group, and a file system path, to any file system request made through the access point. The operating system user and group override any identity information provided by the NFS client. The file system path is exposed as the access point's root directory. Applications using the access point can only access data in its own directory and below. To learn more, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Mounting a File System Using EFS Access Points> .
--
-- This operation requires permissions for the @elasticfilesystem:CreateAccessPoint@ action.
module Network.AWS.EFS.CreateAccessPoint
  ( -- * Creating a request
    CreateAccessPoint (..),
    mkCreateAccessPoint,

    -- ** Request lenses
    capPosixUser,
    capRootDirectory,
    capTags,
    capClientToken,
    capFileSystemId,

    -- * Destructuring the response
    AccessPointDescription (..),
    mkAccessPointDescription,

    -- ** Response lenses
    apdPosixUser,
    apdRootDirectory,
    apdClientToken,
    apdAccessPointId,
    apdFileSystemId,
    apdOwnerId,
    apdName,
    apdAccessPointARN,
    apdLifeCycleState,
    apdTags,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAccessPoint' smart constructor.
data CreateAccessPoint = CreateAccessPoint'
  { posixUser ::
      Lude.Maybe PosixUser,
    rootDirectory :: Lude.Maybe RootDirectory,
    tags :: Lude.Maybe [Tag],
    clientToken :: Lude.Text,
    fileSystemId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccessPoint' with the minimum fields required to make a request.
--
-- * 'clientToken' - A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
-- * 'fileSystemId' - The ID of the EFS file system that the access point provides access to.
-- * 'posixUser' - The operating system user and group applied to all file system requests made using the access point.
-- * 'rootDirectory' - Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
-- * 'tags' - Creates tags associated with the access point. Each tag is a key-value pair.
mkCreateAccessPoint ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'fileSystemId'
  Lude.Text ->
  CreateAccessPoint
mkCreateAccessPoint pClientToken_ pFileSystemId_ =
  CreateAccessPoint'
    { posixUser = Lude.Nothing,
      rootDirectory = Lude.Nothing,
      tags = Lude.Nothing,
      clientToken = pClientToken_,
      fileSystemId = pFileSystemId_
    }

-- | The operating system user and group applied to all file system requests made using the access point.
--
-- /Note:/ Consider using 'posixUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capPosixUser :: Lens.Lens' CreateAccessPoint (Lude.Maybe PosixUser)
capPosixUser = Lens.lens (posixUser :: CreateAccessPoint -> Lude.Maybe PosixUser) (\s a -> s {posixUser = a} :: CreateAccessPoint)
{-# DEPRECATED capPosixUser "Use generic-lens or generic-optics with 'posixUser' instead." #-}

-- | Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capRootDirectory :: Lens.Lens' CreateAccessPoint (Lude.Maybe RootDirectory)
capRootDirectory = Lens.lens (rootDirectory :: CreateAccessPoint -> Lude.Maybe RootDirectory) (\s a -> s {rootDirectory = a} :: CreateAccessPoint)
{-# DEPRECATED capRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | Creates tags associated with the access point. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capTags :: Lens.Lens' CreateAccessPoint (Lude.Maybe [Tag])
capTags = Lens.lens (tags :: CreateAccessPoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAccessPoint)
{-# DEPRECATED capTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capClientToken :: Lens.Lens' CreateAccessPoint Lude.Text
capClientToken = Lens.lens (clientToken :: CreateAccessPoint -> Lude.Text) (\s a -> s {clientToken = a} :: CreateAccessPoint)
{-# DEPRECATED capClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the EFS file system that the access point provides access to.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capFileSystemId :: Lens.Lens' CreateAccessPoint Lude.Text
capFileSystemId = Lens.lens (fileSystemId :: CreateAccessPoint -> Lude.Text) (\s a -> s {fileSystemId = a} :: CreateAccessPoint)
{-# DEPRECATED capFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest CreateAccessPoint where
  type Rs CreateAccessPoint = AccessPointDescription
  request = Req.postJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateAccessPoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateAccessPoint where
  toJSON CreateAccessPoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PosixUser" Lude..=) Lude.<$> posixUser,
            ("RootDirectory" Lude..=) Lude.<$> rootDirectory,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("FileSystemId" Lude..= fileSystemId)
          ]
      )

instance Lude.ToPath CreateAccessPoint where
  toPath = Lude.const "/2015-02-01/access-points"

instance Lude.ToQuery CreateAccessPoint where
  toQuery = Lude.const Lude.mempty
