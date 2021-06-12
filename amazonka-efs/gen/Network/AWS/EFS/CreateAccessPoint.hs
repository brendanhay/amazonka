{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateAccessPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EFS access point. An access point is an application-specific
-- view into an EFS file system that applies an operating system user and
-- group, and a file system path, to any file system request made through
-- the access point. The operating system user and group override any
-- identity information provided by the NFS client. The file system path is
-- exposed as the access point\'s root directory. Applications using the
-- access point can only access data in its own directory and below. To
-- learn more, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Mounting a File System Using EFS Access Points>.
--
-- This operation requires permissions for the
-- @elasticfilesystem:CreateAccessPoint@ action.
module Network.AWS.EFS.CreateAccessPoint
  ( -- * Creating a Request
    CreateAccessPoint (..),
    newCreateAccessPoint,

    -- * Request Lenses
    createAccessPoint_rootDirectory,
    createAccessPoint_posixUser,
    createAccessPoint_tags,
    createAccessPoint_clientToken,
    createAccessPoint_fileSystemId,

    -- * Destructuring the Response
    AccessPointDescription (..),
    newAccessPointDescription,

    -- * Response Lenses
    accessPointDescription_ownerId,
    accessPointDescription_accessPointArn,
    accessPointDescription_accessPointId,
    accessPointDescription_rootDirectory,
    accessPointDescription_name,
    accessPointDescription_posixUser,
    accessPointDescription_tags,
    accessPointDescription_lifeCycleState,
    accessPointDescription_fileSystemId,
    accessPointDescription_clientToken,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAccessPoint' smart constructor.
data CreateAccessPoint = CreateAccessPoint'
  { -- | Specifies the directory on the Amazon EFS file system that the access
    -- point exposes as the root directory of your file system to NFS clients
    -- using the access point. The clients using the access point can only
    -- access the root directory and below. If the @RootDirectory@ > @Path@
    -- specified does not exist, EFS creates it and applies the @CreationInfo@
    -- settings when a client connects to an access point. When specifying a
    -- @RootDirectory@, you need to provide the @Path@, and the @CreationInfo@
    -- is optional.
    rootDirectory :: Core.Maybe RootDirectory,
    -- | The operating system user and group applied to all file system requests
    -- made using the access point.
    posixUser :: Core.Maybe PosixUser,
    -- | Creates tags associated with the access point. Each tag is a key-value
    -- pair.
    tags :: Core.Maybe [Tag],
    -- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure
    -- idempotent creation.
    clientToken :: Core.Text,
    -- | The ID of the EFS file system that the access point provides access to.
    fileSystemId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAccessPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootDirectory', 'createAccessPoint_rootDirectory' - Specifies the directory on the Amazon EFS file system that the access
-- point exposes as the root directory of your file system to NFS clients
-- using the access point. The clients using the access point can only
-- access the root directory and below. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates it and applies the @CreationInfo@
-- settings when a client connects to an access point. When specifying a
-- @RootDirectory@, you need to provide the @Path@, and the @CreationInfo@
-- is optional.
--
-- 'posixUser', 'createAccessPoint_posixUser' - The operating system user and group applied to all file system requests
-- made using the access point.
--
-- 'tags', 'createAccessPoint_tags' - Creates tags associated with the access point. Each tag is a key-value
-- pair.
--
-- 'clientToken', 'createAccessPoint_clientToken' - A string of up to 64 ASCII characters that Amazon EFS uses to ensure
-- idempotent creation.
--
-- 'fileSystemId', 'createAccessPoint_fileSystemId' - The ID of the EFS file system that the access point provides access to.
newCreateAccessPoint ::
  -- | 'clientToken'
  Core.Text ->
  -- | 'fileSystemId'
  Core.Text ->
  CreateAccessPoint
newCreateAccessPoint pClientToken_ pFileSystemId_ =
  CreateAccessPoint'
    { rootDirectory = Core.Nothing,
      posixUser = Core.Nothing,
      tags = Core.Nothing,
      clientToken = pClientToken_,
      fileSystemId = pFileSystemId_
    }

-- | Specifies the directory on the Amazon EFS file system that the access
-- point exposes as the root directory of your file system to NFS clients
-- using the access point. The clients using the access point can only
-- access the root directory and below. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates it and applies the @CreationInfo@
-- settings when a client connects to an access point. When specifying a
-- @RootDirectory@, you need to provide the @Path@, and the @CreationInfo@
-- is optional.
createAccessPoint_rootDirectory :: Lens.Lens' CreateAccessPoint (Core.Maybe RootDirectory)
createAccessPoint_rootDirectory = Lens.lens (\CreateAccessPoint' {rootDirectory} -> rootDirectory) (\s@CreateAccessPoint' {} a -> s {rootDirectory = a} :: CreateAccessPoint)

-- | The operating system user and group applied to all file system requests
-- made using the access point.
createAccessPoint_posixUser :: Lens.Lens' CreateAccessPoint (Core.Maybe PosixUser)
createAccessPoint_posixUser = Lens.lens (\CreateAccessPoint' {posixUser} -> posixUser) (\s@CreateAccessPoint' {} a -> s {posixUser = a} :: CreateAccessPoint)

-- | Creates tags associated with the access point. Each tag is a key-value
-- pair.
createAccessPoint_tags :: Lens.Lens' CreateAccessPoint (Core.Maybe [Tag])
createAccessPoint_tags = Lens.lens (\CreateAccessPoint' {tags} -> tags) (\s@CreateAccessPoint' {} a -> s {tags = a} :: CreateAccessPoint) Core.. Lens.mapping Lens._Coerce

-- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure
-- idempotent creation.
createAccessPoint_clientToken :: Lens.Lens' CreateAccessPoint Core.Text
createAccessPoint_clientToken = Lens.lens (\CreateAccessPoint' {clientToken} -> clientToken) (\s@CreateAccessPoint' {} a -> s {clientToken = a} :: CreateAccessPoint)

-- | The ID of the EFS file system that the access point provides access to.
createAccessPoint_fileSystemId :: Lens.Lens' CreateAccessPoint Core.Text
createAccessPoint_fileSystemId = Lens.lens (\CreateAccessPoint' {fileSystemId} -> fileSystemId) (\s@CreateAccessPoint' {} a -> s {fileSystemId = a} :: CreateAccessPoint)

instance Core.AWSRequest CreateAccessPoint where
  type
    AWSResponse CreateAccessPoint =
      AccessPointDescription
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateAccessPoint

instance Core.NFData CreateAccessPoint

instance Core.ToHeaders CreateAccessPoint where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateAccessPoint where
  toJSON CreateAccessPoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RootDirectory" Core..=) Core.<$> rootDirectory,
            ("PosixUser" Core..=) Core.<$> posixUser,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("FileSystemId" Core..= fileSystemId)
          ]
      )

instance Core.ToPath CreateAccessPoint where
  toPath = Core.const "/2015-02-01/access-points"

instance Core.ToQuery CreateAccessPoint where
  toQuery = Core.const Core.mempty
