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
-- Module      : Amazonka.DataSync.CreateLocationEfs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon EFS file system that DataSync can
-- access for a transfer. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-efs-location.html Creating a location for Amazon EFS>.
module Amazonka.DataSync.CreateLocationEfs
  ( -- * Creating a Request
    CreateLocationEfs (..),
    newCreateLocationEfs,

    -- * Request Lenses
    createLocationEfs_accessPointArn,
    createLocationEfs_fileSystemAccessRoleArn,
    createLocationEfs_inTransitEncryption,
    createLocationEfs_subdirectory,
    createLocationEfs_tags,
    createLocationEfs_efsFilesystemArn,
    createLocationEfs_ec2Config,

    -- * Destructuring the Response
    CreateLocationEfsResponse (..),
    newCreateLocationEfsResponse,

    -- * Response Lenses
    createLocationEfsResponse_locationArn,
    createLocationEfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationEfsRequest
--
-- /See:/ 'newCreateLocationEfs' smart constructor.
data CreateLocationEfs = CreateLocationEfs'
  { -- | Specifies the Amazon Resource Name (ARN) of the access point that
    -- DataSync uses to access the Amazon EFS file system.
    accessPointArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies an Identity and Access Management (IAM) role that DataSync
    -- assumes when mounting the Amazon EFS file system.
    fileSystemAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you want DataSync to use Transport Layer Security
    -- (TLS) 1.2 encryption when it copies data to or from the Amazon EFS file
    -- system.
    --
    -- If you specify an access point using @AccessPointArn@ or an IAM role
    -- using @FileSystemAccessRoleArn@, you must set this parameter to
    -- @TLS1_2@.
    inTransitEncryption :: Prelude.Maybe EfsInTransitEncryption,
    -- | Specifies a mount path for your Amazon EFS file system. This is where
    -- DataSync reads or writes data (depending on if this is a source or
    -- destination location). By default, DataSync uses the root directory, but
    -- you can also include subdirectories.
    --
    -- You must specify a value with forward slashes (for example,
    -- @\/path\/to\/folder@).
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | Specifies the key-value pair that represents a tag that you want to add
    -- to the resource. The value can be an empty string. This value helps you
    -- manage, filter, and search for your resources. We recommend that you
    -- create a name tag for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies the ARN for the Amazon EFS file system.
    efsFilesystemArn :: Prelude.Text,
    -- | Specifies the subnet and security groups DataSync uses to access your
    -- Amazon EFS file system.
    ec2Config :: Ec2Config
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationEfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointArn', 'createLocationEfs_accessPointArn' - Specifies the Amazon Resource Name (ARN) of the access point that
-- DataSync uses to access the Amazon EFS file system.
--
-- 'fileSystemAccessRoleArn', 'createLocationEfs_fileSystemAccessRoleArn' - Specifies an Identity and Access Management (IAM) role that DataSync
-- assumes when mounting the Amazon EFS file system.
--
-- 'inTransitEncryption', 'createLocationEfs_inTransitEncryption' - Specifies whether you want DataSync to use Transport Layer Security
-- (TLS) 1.2 encryption when it copies data to or from the Amazon EFS file
-- system.
--
-- If you specify an access point using @AccessPointArn@ or an IAM role
-- using @FileSystemAccessRoleArn@, you must set this parameter to
-- @TLS1_2@.
--
-- 'subdirectory', 'createLocationEfs_subdirectory' - Specifies a mount path for your Amazon EFS file system. This is where
-- DataSync reads or writes data (depending on if this is a source or
-- destination location). By default, DataSync uses the root directory, but
-- you can also include subdirectories.
--
-- You must specify a value with forward slashes (for example,
-- @\/path\/to\/folder@).
--
-- 'tags', 'createLocationEfs_tags' - Specifies the key-value pair that represents a tag that you want to add
-- to the resource. The value can be an empty string. This value helps you
-- manage, filter, and search for your resources. We recommend that you
-- create a name tag for your location.
--
-- 'efsFilesystemArn', 'createLocationEfs_efsFilesystemArn' - Specifies the ARN for the Amazon EFS file system.
--
-- 'ec2Config', 'createLocationEfs_ec2Config' - Specifies the subnet and security groups DataSync uses to access your
-- Amazon EFS file system.
newCreateLocationEfs ::
  -- | 'efsFilesystemArn'
  Prelude.Text ->
  -- | 'ec2Config'
  Ec2Config ->
  CreateLocationEfs
newCreateLocationEfs pEfsFilesystemArn_ pEc2Config_ =
  CreateLocationEfs'
    { accessPointArn =
        Prelude.Nothing,
      fileSystemAccessRoleArn = Prelude.Nothing,
      inTransitEncryption = Prelude.Nothing,
      subdirectory = Prelude.Nothing,
      tags = Prelude.Nothing,
      efsFilesystemArn = pEfsFilesystemArn_,
      ec2Config = pEc2Config_
    }

-- | Specifies the Amazon Resource Name (ARN) of the access point that
-- DataSync uses to access the Amazon EFS file system.
createLocationEfs_accessPointArn :: Lens.Lens' CreateLocationEfs (Prelude.Maybe Prelude.Text)
createLocationEfs_accessPointArn = Lens.lens (\CreateLocationEfs' {accessPointArn} -> accessPointArn) (\s@CreateLocationEfs' {} a -> s {accessPointArn = a} :: CreateLocationEfs)

-- | Specifies an Identity and Access Management (IAM) role that DataSync
-- assumes when mounting the Amazon EFS file system.
createLocationEfs_fileSystemAccessRoleArn :: Lens.Lens' CreateLocationEfs (Prelude.Maybe Prelude.Text)
createLocationEfs_fileSystemAccessRoleArn = Lens.lens (\CreateLocationEfs' {fileSystemAccessRoleArn} -> fileSystemAccessRoleArn) (\s@CreateLocationEfs' {} a -> s {fileSystemAccessRoleArn = a} :: CreateLocationEfs)

-- | Specifies whether you want DataSync to use Transport Layer Security
-- (TLS) 1.2 encryption when it copies data to or from the Amazon EFS file
-- system.
--
-- If you specify an access point using @AccessPointArn@ or an IAM role
-- using @FileSystemAccessRoleArn@, you must set this parameter to
-- @TLS1_2@.
createLocationEfs_inTransitEncryption :: Lens.Lens' CreateLocationEfs (Prelude.Maybe EfsInTransitEncryption)
createLocationEfs_inTransitEncryption = Lens.lens (\CreateLocationEfs' {inTransitEncryption} -> inTransitEncryption) (\s@CreateLocationEfs' {} a -> s {inTransitEncryption = a} :: CreateLocationEfs)

-- | Specifies a mount path for your Amazon EFS file system. This is where
-- DataSync reads or writes data (depending on if this is a source or
-- destination location). By default, DataSync uses the root directory, but
-- you can also include subdirectories.
--
-- You must specify a value with forward slashes (for example,
-- @\/path\/to\/folder@).
createLocationEfs_subdirectory :: Lens.Lens' CreateLocationEfs (Prelude.Maybe Prelude.Text)
createLocationEfs_subdirectory = Lens.lens (\CreateLocationEfs' {subdirectory} -> subdirectory) (\s@CreateLocationEfs' {} a -> s {subdirectory = a} :: CreateLocationEfs)

-- | Specifies the key-value pair that represents a tag that you want to add
-- to the resource. The value can be an empty string. This value helps you
-- manage, filter, and search for your resources. We recommend that you
-- create a name tag for your location.
createLocationEfs_tags :: Lens.Lens' CreateLocationEfs (Prelude.Maybe [TagListEntry])
createLocationEfs_tags = Lens.lens (\CreateLocationEfs' {tags} -> tags) (\s@CreateLocationEfs' {} a -> s {tags = a} :: CreateLocationEfs) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the ARN for the Amazon EFS file system.
createLocationEfs_efsFilesystemArn :: Lens.Lens' CreateLocationEfs Prelude.Text
createLocationEfs_efsFilesystemArn = Lens.lens (\CreateLocationEfs' {efsFilesystemArn} -> efsFilesystemArn) (\s@CreateLocationEfs' {} a -> s {efsFilesystemArn = a} :: CreateLocationEfs)

-- | Specifies the subnet and security groups DataSync uses to access your
-- Amazon EFS file system.
createLocationEfs_ec2Config :: Lens.Lens' CreateLocationEfs Ec2Config
createLocationEfs_ec2Config = Lens.lens (\CreateLocationEfs' {ec2Config} -> ec2Config) (\s@CreateLocationEfs' {} a -> s {ec2Config = a} :: CreateLocationEfs)

instance Core.AWSRequest CreateLocationEfs where
  type
    AWSResponse CreateLocationEfs =
      CreateLocationEfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationEfsResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationEfs where
  hashWithSalt _salt CreateLocationEfs' {..} =
    _salt
      `Prelude.hashWithSalt` accessPointArn
      `Prelude.hashWithSalt` fileSystemAccessRoleArn
      `Prelude.hashWithSalt` inTransitEncryption
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` efsFilesystemArn
      `Prelude.hashWithSalt` ec2Config

instance Prelude.NFData CreateLocationEfs where
  rnf CreateLocationEfs' {..} =
    Prelude.rnf accessPointArn
      `Prelude.seq` Prelude.rnf fileSystemAccessRoleArn
      `Prelude.seq` Prelude.rnf inTransitEncryption
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf efsFilesystemArn
      `Prelude.seq` Prelude.rnf ec2Config

instance Data.ToHeaders CreateLocationEfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationEfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationEfs where
  toJSON CreateLocationEfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessPointArn" Data..=)
              Prelude.<$> accessPointArn,
            ("FileSystemAccessRoleArn" Data..=)
              Prelude.<$> fileSystemAccessRoleArn,
            ("InTransitEncryption" Data..=)
              Prelude.<$> inTransitEncryption,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("EfsFilesystemArn" Data..= efsFilesystemArn),
            Prelude.Just ("Ec2Config" Data..= ec2Config)
          ]
      )

instance Data.ToPath CreateLocationEfs where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationEfs where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationEfs
--
-- /See:/ 'newCreateLocationEfsResponse' smart constructor.
data CreateLocationEfsResponse = CreateLocationEfsResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS file system location
    -- that you create.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationEfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationEfsResponse_locationArn' - The Amazon Resource Name (ARN) of the Amazon EFS file system location
-- that you create.
--
-- 'httpStatus', 'createLocationEfsResponse_httpStatus' - The response's http status code.
newCreateLocationEfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationEfsResponse
newCreateLocationEfsResponse pHttpStatus_ =
  CreateLocationEfsResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Amazon EFS file system location
-- that you create.
createLocationEfsResponse_locationArn :: Lens.Lens' CreateLocationEfsResponse (Prelude.Maybe Prelude.Text)
createLocationEfsResponse_locationArn = Lens.lens (\CreateLocationEfsResponse' {locationArn} -> locationArn) (\s@CreateLocationEfsResponse' {} a -> s {locationArn = a} :: CreateLocationEfsResponse)

-- | The response's http status code.
createLocationEfsResponse_httpStatus :: Lens.Lens' CreateLocationEfsResponse Prelude.Int
createLocationEfsResponse_httpStatus = Lens.lens (\CreateLocationEfsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationEfsResponse' {} a -> s {httpStatus = a} :: CreateLocationEfsResponse)

instance Prelude.NFData CreateLocationEfsResponse where
  rnf CreateLocationEfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
