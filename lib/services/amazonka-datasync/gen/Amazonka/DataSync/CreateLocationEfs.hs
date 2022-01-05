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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon EFS file system.
module Amazonka.DataSync.CreateLocationEfs
  ( -- * Creating a Request
    CreateLocationEfs (..),
    newCreateLocationEfs,

    -- * Request Lenses
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
import Amazonka.DataSync.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationEfsRequest
--
-- /See:/ 'newCreateLocationEfs' smart constructor.
data CreateLocationEfs = CreateLocationEfs'
  { -- | A subdirectory in the location’s path. This subdirectory in the EFS file
    -- system is used to read data from the EFS source location or write data
    -- to the EFS destination. By default, DataSync uses the root directory.
    --
    -- @Subdirectory@ must be specified with forward slashes. For example,
    -- @\/path\/to\/folder@.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that represents a tag that you want to add to the
    -- resource. The value can be an empty string. This value helps you manage,
    -- filter, and search for your resources. We recommend that you create a
    -- name tag for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The Amazon Resource Name (ARN) for the Amazon EFS file system.
    efsFilesystemArn :: Prelude.Text,
    -- | The subnet and security group that the Amazon EFS file system uses. The
    -- security group that you provide needs to be able to communicate with the
    -- security group on the mount target in the subnet specified.
    --
    -- The exact relationship between security group M (of the mount target)
    -- and security group S (which you provide for DataSync to use at this
    -- stage) is as follows:
    --
    -- -   Security group M (which you associate with the mount target) must
    --     allow inbound access for the Transmission Control Protocol (TCP) on
    --     the NFS port (2049) from security group S. You can enable inbound
    --     connections either by IP address (CIDR range) or security group.
    --
    -- -   Security group S (provided to DataSync to access EFS) should have a
    --     rule that enables outbound connections to the NFS port on one of the
    --     file system’s mount targets. You can enable outbound connections
    --     either by IP address (CIDR range) or security group.
    --
    --     For information about security groups and mount targets, see
    --     Security Groups for Amazon EC2 Instances and Mount Targets in the
    --     /Amazon EFS User Guide./
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
-- 'subdirectory', 'createLocationEfs_subdirectory' - A subdirectory in the location’s path. This subdirectory in the EFS file
-- system is used to read data from the EFS source location or write data
-- to the EFS destination. By default, DataSync uses the root directory.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
--
-- 'tags', 'createLocationEfs_tags' - The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
--
-- 'efsFilesystemArn', 'createLocationEfs_efsFilesystemArn' - The Amazon Resource Name (ARN) for the Amazon EFS file system.
--
-- 'ec2Config', 'createLocationEfs_ec2Config' - The subnet and security group that the Amazon EFS file system uses. The
-- security group that you provide needs to be able to communicate with the
-- security group on the mount target in the subnet specified.
--
-- The exact relationship between security group M (of the mount target)
-- and security group S (which you provide for DataSync to use at this
-- stage) is as follows:
--
-- -   Security group M (which you associate with the mount target) must
--     allow inbound access for the Transmission Control Protocol (TCP) on
--     the NFS port (2049) from security group S. You can enable inbound
--     connections either by IP address (CIDR range) or security group.
--
-- -   Security group S (provided to DataSync to access EFS) should have a
--     rule that enables outbound connections to the NFS port on one of the
--     file system’s mount targets. You can enable outbound connections
--     either by IP address (CIDR range) or security group.
--
--     For information about security groups and mount targets, see
--     Security Groups for Amazon EC2 Instances and Mount Targets in the
--     /Amazon EFS User Guide./
newCreateLocationEfs ::
  -- | 'efsFilesystemArn'
  Prelude.Text ->
  -- | 'ec2Config'
  Ec2Config ->
  CreateLocationEfs
newCreateLocationEfs pEfsFilesystemArn_ pEc2Config_ =
  CreateLocationEfs'
    { subdirectory = Prelude.Nothing,
      tags = Prelude.Nothing,
      efsFilesystemArn = pEfsFilesystemArn_,
      ec2Config = pEc2Config_
    }

-- | A subdirectory in the location’s path. This subdirectory in the EFS file
-- system is used to read data from the EFS source location or write data
-- to the EFS destination. By default, DataSync uses the root directory.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
createLocationEfs_subdirectory :: Lens.Lens' CreateLocationEfs (Prelude.Maybe Prelude.Text)
createLocationEfs_subdirectory = Lens.lens (\CreateLocationEfs' {subdirectory} -> subdirectory) (\s@CreateLocationEfs' {} a -> s {subdirectory = a} :: CreateLocationEfs)

-- | The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
createLocationEfs_tags :: Lens.Lens' CreateLocationEfs (Prelude.Maybe [TagListEntry])
createLocationEfs_tags = Lens.lens (\CreateLocationEfs' {tags} -> tags) (\s@CreateLocationEfs' {} a -> s {tags = a} :: CreateLocationEfs) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the Amazon EFS file system.
createLocationEfs_efsFilesystemArn :: Lens.Lens' CreateLocationEfs Prelude.Text
createLocationEfs_efsFilesystemArn = Lens.lens (\CreateLocationEfs' {efsFilesystemArn} -> efsFilesystemArn) (\s@CreateLocationEfs' {} a -> s {efsFilesystemArn = a} :: CreateLocationEfs)

-- | The subnet and security group that the Amazon EFS file system uses. The
-- security group that you provide needs to be able to communicate with the
-- security group on the mount target in the subnet specified.
--
-- The exact relationship between security group M (of the mount target)
-- and security group S (which you provide for DataSync to use at this
-- stage) is as follows:
--
-- -   Security group M (which you associate with the mount target) must
--     allow inbound access for the Transmission Control Protocol (TCP) on
--     the NFS port (2049) from security group S. You can enable inbound
--     connections either by IP address (CIDR range) or security group.
--
-- -   Security group S (provided to DataSync to access EFS) should have a
--     rule that enables outbound connections to the NFS port on one of the
--     file system’s mount targets. You can enable outbound connections
--     either by IP address (CIDR range) or security group.
--
--     For information about security groups and mount targets, see
--     Security Groups for Amazon EC2 Instances and Mount Targets in the
--     /Amazon EFS User Guide./
createLocationEfs_ec2Config :: Lens.Lens' CreateLocationEfs Ec2Config
createLocationEfs_ec2Config = Lens.lens (\CreateLocationEfs' {ec2Config} -> ec2Config) (\s@CreateLocationEfs' {} a -> s {ec2Config = a} :: CreateLocationEfs)

instance Core.AWSRequest CreateLocationEfs where
  type
    AWSResponse CreateLocationEfs =
      CreateLocationEfsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationEfsResponse'
            Prelude.<$> (x Core..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationEfs where
  hashWithSalt _salt CreateLocationEfs' {..} =
    _salt `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` efsFilesystemArn
      `Prelude.hashWithSalt` ec2Config

instance Prelude.NFData CreateLocationEfs where
  rnf CreateLocationEfs' {..} =
    Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf efsFilesystemArn
      `Prelude.seq` Prelude.rnf ec2Config

instance Core.ToHeaders CreateLocationEfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.CreateLocationEfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLocationEfs where
  toJSON CreateLocationEfs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Subdirectory" Core..=) Prelude.<$> subdirectory,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("EfsFilesystemArn" Core..= efsFilesystemArn),
            Prelude.Just ("Ec2Config" Core..= ec2Config)
          ]
      )

instance Core.ToPath CreateLocationEfs where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLocationEfs where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationEfs
--
-- /See:/ 'newCreateLocationEfsResponse' smart constructor.
data CreateLocationEfsResponse = CreateLocationEfsResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS file system location
    -- that is created.
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
-- that is created.
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
-- that is created.
createLocationEfsResponse_locationArn :: Lens.Lens' CreateLocationEfsResponse (Prelude.Maybe Prelude.Text)
createLocationEfsResponse_locationArn = Lens.lens (\CreateLocationEfsResponse' {locationArn} -> locationArn) (\s@CreateLocationEfsResponse' {} a -> s {locationArn = a} :: CreateLocationEfsResponse)

-- | The response's http status code.
createLocationEfsResponse_httpStatus :: Lens.Lens' CreateLocationEfsResponse Prelude.Int
createLocationEfsResponse_httpStatus = Lens.lens (\CreateLocationEfsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationEfsResponse' {} a -> s {httpStatus = a} :: CreateLocationEfsResponse)

instance Prelude.NFData CreateLocationEfsResponse where
  rnf CreateLocationEfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
