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
-- Module      : Amazonka.EFS.CreateAccessPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- access point can only access data in the application\'s own directory
-- and any subdirectories. To learn more, see
-- <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Mounting a file system using EFS access points>.
--
-- This operation requires permissions for the
-- @elasticfilesystem:CreateAccessPoint@ action.
module Amazonka.EFS.CreateAccessPoint
  ( -- * Creating a Request
    CreateAccessPoint (..),
    newCreateAccessPoint,

    -- * Request Lenses
    createAccessPoint_tags,
    createAccessPoint_posixUser,
    createAccessPoint_rootDirectory,
    createAccessPoint_clientToken,
    createAccessPoint_fileSystemId,

    -- * Destructuring the Response
    AccessPointDescription (..),
    newAccessPointDescription,

    -- * Response Lenses
    accessPointDescription_tags,
    accessPointDescription_clientToken,
    accessPointDescription_name,
    accessPointDescription_ownerId,
    accessPointDescription_accessPointArn,
    accessPointDescription_posixUser,
    accessPointDescription_fileSystemId,
    accessPointDescription_accessPointId,
    accessPointDescription_rootDirectory,
    accessPointDescription_lifeCycleState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessPoint' smart constructor.
data CreateAccessPoint = CreateAccessPoint'
  { -- | Creates tags associated with the access point. Each tag is a key-value
    -- pair, each key must be unique. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The operating system user and group applied to all file system requests
    -- made using the access point.
    posixUser :: Prelude.Maybe PosixUser,
    -- | Specifies the directory on the Amazon EFS file system that the access
    -- point exposes as the root directory of your file system to NFS clients
    -- using the access point. The clients using the access point can only
    -- access the root directory and below. If the @RootDirectory@ > @Path@
    -- specified does not exist, EFS creates it and applies the @CreationInfo@
    -- settings when a client connects to an access point. When specifying a
    -- @RootDirectory@, you must provide the @Path@, and the @CreationInfo@.
    --
    -- Amazon EFS creates a root directory only if you have provided the
    -- CreationInfo: OwnUid, OwnGID, and permissions for the directory. If you
    -- do not provide this information, Amazon EFS does not create the root
    -- directory. If the root directory does not exist, attempts to mount using
    -- the access point will fail.
    rootDirectory :: Prelude.Maybe RootDirectory,
    -- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure
    -- idempotent creation.
    clientToken :: Prelude.Text,
    -- | The ID of the EFS file system that the access point provides access to.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAccessPoint_tags' - Creates tags associated with the access point. Each tag is a key-value
-- pair, each key must be unique. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'posixUser', 'createAccessPoint_posixUser' - The operating system user and group applied to all file system requests
-- made using the access point.
--
-- 'rootDirectory', 'createAccessPoint_rootDirectory' - Specifies the directory on the Amazon EFS file system that the access
-- point exposes as the root directory of your file system to NFS clients
-- using the access point. The clients using the access point can only
-- access the root directory and below. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates it and applies the @CreationInfo@
-- settings when a client connects to an access point. When specifying a
-- @RootDirectory@, you must provide the @Path@, and the @CreationInfo@.
--
-- Amazon EFS creates a root directory only if you have provided the
-- CreationInfo: OwnUid, OwnGID, and permissions for the directory. If you
-- do not provide this information, Amazon EFS does not create the root
-- directory. If the root directory does not exist, attempts to mount using
-- the access point will fail.
--
-- 'clientToken', 'createAccessPoint_clientToken' - A string of up to 64 ASCII characters that Amazon EFS uses to ensure
-- idempotent creation.
--
-- 'fileSystemId', 'createAccessPoint_fileSystemId' - The ID of the EFS file system that the access point provides access to.
newCreateAccessPoint ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'fileSystemId'
  Prelude.Text ->
  CreateAccessPoint
newCreateAccessPoint pClientToken_ pFileSystemId_ =
  CreateAccessPoint'
    { tags = Prelude.Nothing,
      posixUser = Prelude.Nothing,
      rootDirectory = Prelude.Nothing,
      clientToken = pClientToken_,
      fileSystemId = pFileSystemId_
    }

-- | Creates tags associated with the access point. Each tag is a key-value
-- pair, each key must be unique. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
createAccessPoint_tags :: Lens.Lens' CreateAccessPoint (Prelude.Maybe [Tag])
createAccessPoint_tags = Lens.lens (\CreateAccessPoint' {tags} -> tags) (\s@CreateAccessPoint' {} a -> s {tags = a} :: CreateAccessPoint) Prelude.. Lens.mapping Lens.coerced

-- | The operating system user and group applied to all file system requests
-- made using the access point.
createAccessPoint_posixUser :: Lens.Lens' CreateAccessPoint (Prelude.Maybe PosixUser)
createAccessPoint_posixUser = Lens.lens (\CreateAccessPoint' {posixUser} -> posixUser) (\s@CreateAccessPoint' {} a -> s {posixUser = a} :: CreateAccessPoint)

-- | Specifies the directory on the Amazon EFS file system that the access
-- point exposes as the root directory of your file system to NFS clients
-- using the access point. The clients using the access point can only
-- access the root directory and below. If the @RootDirectory@ > @Path@
-- specified does not exist, EFS creates it and applies the @CreationInfo@
-- settings when a client connects to an access point. When specifying a
-- @RootDirectory@, you must provide the @Path@, and the @CreationInfo@.
--
-- Amazon EFS creates a root directory only if you have provided the
-- CreationInfo: OwnUid, OwnGID, and permissions for the directory. If you
-- do not provide this information, Amazon EFS does not create the root
-- directory. If the root directory does not exist, attempts to mount using
-- the access point will fail.
createAccessPoint_rootDirectory :: Lens.Lens' CreateAccessPoint (Prelude.Maybe RootDirectory)
createAccessPoint_rootDirectory = Lens.lens (\CreateAccessPoint' {rootDirectory} -> rootDirectory) (\s@CreateAccessPoint' {} a -> s {rootDirectory = a} :: CreateAccessPoint)

-- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure
-- idempotent creation.
createAccessPoint_clientToken :: Lens.Lens' CreateAccessPoint Prelude.Text
createAccessPoint_clientToken = Lens.lens (\CreateAccessPoint' {clientToken} -> clientToken) (\s@CreateAccessPoint' {} a -> s {clientToken = a} :: CreateAccessPoint)

-- | The ID of the EFS file system that the access point provides access to.
createAccessPoint_fileSystemId :: Lens.Lens' CreateAccessPoint Prelude.Text
createAccessPoint_fileSystemId = Lens.lens (\CreateAccessPoint' {fileSystemId} -> fileSystemId) (\s@CreateAccessPoint' {} a -> s {fileSystemId = a} :: CreateAccessPoint)

instance Core.AWSRequest CreateAccessPoint where
  type
    AWSResponse CreateAccessPoint =
      AccessPointDescription
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateAccessPoint where
  hashWithSalt _salt CreateAccessPoint' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` posixUser
      `Prelude.hashWithSalt` rootDirectory
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData CreateAccessPoint where
  rnf CreateAccessPoint' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf posixUser
      `Prelude.seq` Prelude.rnf rootDirectory
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToHeaders CreateAccessPoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAccessPoint where
  toJSON CreateAccessPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("PosixUser" Data..=) Prelude.<$> posixUser,
            ("RootDirectory" Data..=) Prelude.<$> rootDirectory,
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("FileSystemId" Data..= fileSystemId)
          ]
      )

instance Data.ToPath CreateAccessPoint where
  toPath = Prelude.const "/2015-02-01/access-points"

instance Data.ToQuery CreateAccessPoint where
  toQuery = Prelude.const Prelude.mempty
