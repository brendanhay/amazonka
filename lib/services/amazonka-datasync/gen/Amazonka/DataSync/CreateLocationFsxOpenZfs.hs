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
-- Module      : Amazonka.DataSync.CreateLocationFsxOpenZfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon FSx for OpenZFS file system that
-- DataSync can access for a transfer. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-openzfs-location.html Creating a location for FSx for OpenZFS>.
--
-- Request parameters related to @SMB@ aren\'t supported with the
-- @CreateLocationFsxOpenZfs@ operation.
module Amazonka.DataSync.CreateLocationFsxOpenZfs
  ( -- * Creating a Request
    CreateLocationFsxOpenZfs (..),
    newCreateLocationFsxOpenZfs,

    -- * Request Lenses
    createLocationFsxOpenZfs_subdirectory,
    createLocationFsxOpenZfs_tags,
    createLocationFsxOpenZfs_fsxFilesystemArn,
    createLocationFsxOpenZfs_protocol,
    createLocationFsxOpenZfs_securityGroupArns,

    -- * Destructuring the Response
    CreateLocationFsxOpenZfsResponse (..),
    newCreateLocationFsxOpenZfsResponse,

    -- * Response Lenses
    createLocationFsxOpenZfsResponse_locationArn,
    createLocationFsxOpenZfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocationFsxOpenZfs' smart constructor.
data CreateLocationFsxOpenZfs = CreateLocationFsxOpenZfs'
  { -- | A subdirectory in the location\'s path that must begin with @\/fsx@.
    -- DataSync uses this subdirectory to read or write data (depending on
    -- whether the file system is a source or destination location).
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that represents a tag that you want to add to the
    -- resource. The value can be an empty string. This value helps you manage,
    -- filter, and search for your resources. We recommend that you create a
    -- name tag for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The Amazon Resource Name (ARN) of the FSx for OpenZFS file system.
    fsxFilesystemArn :: Prelude.Text,
    -- | The type of protocol that DataSync uses to access your file system.
    protocol :: FsxProtocol,
    -- | The ARNs of the security groups that are used to configure the FSx for
    -- OpenZFS file system.
    securityGroupArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxOpenZfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdirectory', 'createLocationFsxOpenZfs_subdirectory' - A subdirectory in the location\'s path that must begin with @\/fsx@.
-- DataSync uses this subdirectory to read or write data (depending on
-- whether the file system is a source or destination location).
--
-- 'tags', 'createLocationFsxOpenZfs_tags' - The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
--
-- 'fsxFilesystemArn', 'createLocationFsxOpenZfs_fsxFilesystemArn' - The Amazon Resource Name (ARN) of the FSx for OpenZFS file system.
--
-- 'protocol', 'createLocationFsxOpenZfs_protocol' - The type of protocol that DataSync uses to access your file system.
--
-- 'securityGroupArns', 'createLocationFsxOpenZfs_securityGroupArns' - The ARNs of the security groups that are used to configure the FSx for
-- OpenZFS file system.
newCreateLocationFsxOpenZfs ::
  -- | 'fsxFilesystemArn'
  Prelude.Text ->
  -- | 'protocol'
  FsxProtocol ->
  -- | 'securityGroupArns'
  Prelude.NonEmpty Prelude.Text ->
  CreateLocationFsxOpenZfs
newCreateLocationFsxOpenZfs
  pFsxFilesystemArn_
  pProtocol_
  pSecurityGroupArns_ =
    CreateLocationFsxOpenZfs'
      { subdirectory =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        fsxFilesystemArn = pFsxFilesystemArn_,
        protocol = pProtocol_,
        securityGroupArns =
          Lens.coerced Lens.# pSecurityGroupArns_
      }

-- | A subdirectory in the location\'s path that must begin with @\/fsx@.
-- DataSync uses this subdirectory to read or write data (depending on
-- whether the file system is a source or destination location).
createLocationFsxOpenZfs_subdirectory :: Lens.Lens' CreateLocationFsxOpenZfs (Prelude.Maybe Prelude.Text)
createLocationFsxOpenZfs_subdirectory = Lens.lens (\CreateLocationFsxOpenZfs' {subdirectory} -> subdirectory) (\s@CreateLocationFsxOpenZfs' {} a -> s {subdirectory = a} :: CreateLocationFsxOpenZfs)

-- | The key-value pair that represents a tag that you want to add to the
-- resource. The value can be an empty string. This value helps you manage,
-- filter, and search for your resources. We recommend that you create a
-- name tag for your location.
createLocationFsxOpenZfs_tags :: Lens.Lens' CreateLocationFsxOpenZfs (Prelude.Maybe [TagListEntry])
createLocationFsxOpenZfs_tags = Lens.lens (\CreateLocationFsxOpenZfs' {tags} -> tags) (\s@CreateLocationFsxOpenZfs' {} a -> s {tags = a} :: CreateLocationFsxOpenZfs) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the FSx for OpenZFS file system.
createLocationFsxOpenZfs_fsxFilesystemArn :: Lens.Lens' CreateLocationFsxOpenZfs Prelude.Text
createLocationFsxOpenZfs_fsxFilesystemArn = Lens.lens (\CreateLocationFsxOpenZfs' {fsxFilesystemArn} -> fsxFilesystemArn) (\s@CreateLocationFsxOpenZfs' {} a -> s {fsxFilesystemArn = a} :: CreateLocationFsxOpenZfs)

-- | The type of protocol that DataSync uses to access your file system.
createLocationFsxOpenZfs_protocol :: Lens.Lens' CreateLocationFsxOpenZfs FsxProtocol
createLocationFsxOpenZfs_protocol = Lens.lens (\CreateLocationFsxOpenZfs' {protocol} -> protocol) (\s@CreateLocationFsxOpenZfs' {} a -> s {protocol = a} :: CreateLocationFsxOpenZfs)

-- | The ARNs of the security groups that are used to configure the FSx for
-- OpenZFS file system.
createLocationFsxOpenZfs_securityGroupArns :: Lens.Lens' CreateLocationFsxOpenZfs (Prelude.NonEmpty Prelude.Text)
createLocationFsxOpenZfs_securityGroupArns = Lens.lens (\CreateLocationFsxOpenZfs' {securityGroupArns} -> securityGroupArns) (\s@CreateLocationFsxOpenZfs' {} a -> s {securityGroupArns = a} :: CreateLocationFsxOpenZfs) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLocationFsxOpenZfs where
  type
    AWSResponse CreateLocationFsxOpenZfs =
      CreateLocationFsxOpenZfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationFsxOpenZfsResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationFsxOpenZfs where
  hashWithSalt _salt CreateLocationFsxOpenZfs' {..} =
    _salt `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fsxFilesystemArn
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` securityGroupArns

instance Prelude.NFData CreateLocationFsxOpenZfs where
  rnf CreateLocationFsxOpenZfs' {..} =
    Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fsxFilesystemArn
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf securityGroupArns

instance Data.ToHeaders CreateLocationFsxOpenZfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationFsxOpenZfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationFsxOpenZfs where
  toJSON CreateLocationFsxOpenZfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FsxFilesystemArn" Data..= fsxFilesystemArn),
            Prelude.Just ("Protocol" Data..= protocol),
            Prelude.Just
              ("SecurityGroupArns" Data..= securityGroupArns)
          ]
      )

instance Data.ToPath CreateLocationFsxOpenZfs where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationFsxOpenZfs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLocationFsxOpenZfsResponse' smart constructor.
data CreateLocationFsxOpenZfsResponse = CreateLocationFsxOpenZfsResponse'
  { -- | The ARN of the FSx for OpenZFS file system location that you created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxOpenZfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationFsxOpenZfsResponse_locationArn' - The ARN of the FSx for OpenZFS file system location that you created.
--
-- 'httpStatus', 'createLocationFsxOpenZfsResponse_httpStatus' - The response's http status code.
newCreateLocationFsxOpenZfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationFsxOpenZfsResponse
newCreateLocationFsxOpenZfsResponse pHttpStatus_ =
  CreateLocationFsxOpenZfsResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the FSx for OpenZFS file system location that you created.
createLocationFsxOpenZfsResponse_locationArn :: Lens.Lens' CreateLocationFsxOpenZfsResponse (Prelude.Maybe Prelude.Text)
createLocationFsxOpenZfsResponse_locationArn = Lens.lens (\CreateLocationFsxOpenZfsResponse' {locationArn} -> locationArn) (\s@CreateLocationFsxOpenZfsResponse' {} a -> s {locationArn = a} :: CreateLocationFsxOpenZfsResponse)

-- | The response's http status code.
createLocationFsxOpenZfsResponse_httpStatus :: Lens.Lens' CreateLocationFsxOpenZfsResponse Prelude.Int
createLocationFsxOpenZfsResponse_httpStatus = Lens.lens (\CreateLocationFsxOpenZfsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationFsxOpenZfsResponse' {} a -> s {httpStatus = a} :: CreateLocationFsxOpenZfsResponse)

instance
  Prelude.NFData
    CreateLocationFsxOpenZfsResponse
  where
  rnf CreateLocationFsxOpenZfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
