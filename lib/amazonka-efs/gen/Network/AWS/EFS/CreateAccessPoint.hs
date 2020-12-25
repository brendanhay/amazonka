{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    capClientToken,
    capFileSystemId,
    capPosixUser,
    capRootDirectory,
    capTags,

    -- * Destructuring the response
    Types.AccessPointDescription (..),
    Types.mkAccessPointDescription,

    -- ** Response lenses
    Types.apdAccessPointArn,
    Types.apdAccessPointId,
    Types.apdClientToken,
    Types.apdFileSystemId,
    Types.apdLifeCycleState,
    Types.apdName,
    Types.apdOwnerId,
    Types.apdPosixUser,
    Types.apdRootDirectory,
    Types.apdTags,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAccessPoint' smart constructor.
data CreateAccessPoint = CreateAccessPoint'
  { -- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
    clientToken :: Types.ClientToken,
    -- | The ID of the EFS file system that the access point provides access to.
    fileSystemId :: Types.FileSystemId,
    -- | The operating system user and group applied to all file system requests made using the access point.
    posixUser :: Core.Maybe Types.PosixUser,
    -- | Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
    rootDirectory :: Core.Maybe Types.RootDirectory,
    -- | Creates tags associated with the access point. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccessPoint' value with any optional fields omitted.
mkCreateAccessPoint ::
  -- | 'clientToken'
  Types.ClientToken ->
  -- | 'fileSystemId'
  Types.FileSystemId ->
  CreateAccessPoint
mkCreateAccessPoint clientToken fileSystemId =
  CreateAccessPoint'
    { clientToken,
      fileSystemId,
      posixUser = Core.Nothing,
      rootDirectory = Core.Nothing,
      tags = Core.Nothing
    }

-- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capClientToken :: Lens.Lens' CreateAccessPoint Types.ClientToken
capClientToken = Lens.field @"clientToken"
{-# DEPRECATED capClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the EFS file system that the access point provides access to.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capFileSystemId :: Lens.Lens' CreateAccessPoint Types.FileSystemId
capFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED capFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The operating system user and group applied to all file system requests made using the access point.
--
-- /Note:/ Consider using 'posixUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capPosixUser :: Lens.Lens' CreateAccessPoint (Core.Maybe Types.PosixUser)
capPosixUser = Lens.field @"posixUser"
{-# DEPRECATED capPosixUser "Use generic-lens or generic-optics with 'posixUser' instead." #-}

-- | Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
--
-- /Note:/ Consider using 'rootDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capRootDirectory :: Lens.Lens' CreateAccessPoint (Core.Maybe Types.RootDirectory)
capRootDirectory = Lens.field @"rootDirectory"
{-# DEPRECATED capRootDirectory "Use generic-lens or generic-optics with 'rootDirectory' instead." #-}

-- | Creates tags associated with the access point. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capTags :: Lens.Lens' CreateAccessPoint (Core.Maybe [Types.Tag])
capTags = Lens.field @"tags"
{-# DEPRECATED capTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateAccessPoint where
  toJSON CreateAccessPoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("FileSystemId" Core..= fileSystemId),
            ("PosixUser" Core..=) Core.<$> posixUser,
            ("RootDirectory" Core..=) Core.<$> rootDirectory,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateAccessPoint where
  type Rs CreateAccessPoint = Types.AccessPointDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-02-01/access-points",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
