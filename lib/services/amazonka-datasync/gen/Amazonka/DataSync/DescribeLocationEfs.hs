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
-- Module      : Amazonka.DataSync.DescribeLocationEfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about your DataSync location for an Amazon EFS file
-- system.
module Amazonka.DataSync.DescribeLocationEfs
  ( -- * Creating a Request
    DescribeLocationEfs (..),
    newDescribeLocationEfs,

    -- * Request Lenses
    describeLocationEfs_locationArn,

    -- * Destructuring the Response
    DescribeLocationEfsResponse (..),
    newDescribeLocationEfsResponse,

    -- * Response Lenses
    describeLocationEfsResponse_inTransitEncryption,
    describeLocationEfsResponse_accessPointArn,
    describeLocationEfsResponse_fileSystemAccessRoleArn,
    describeLocationEfsResponse_locationArn,
    describeLocationEfsResponse_ec2Config,
    describeLocationEfsResponse_locationUri,
    describeLocationEfsResponse_creationTime,
    describeLocationEfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationEfsRequest
--
-- /See:/ 'newDescribeLocationEfs' smart constructor.
data DescribeLocationEfs = DescribeLocationEfs'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS file system location
    -- that you want information about.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationEfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationEfs_locationArn' - The Amazon Resource Name (ARN) of the Amazon EFS file system location
-- that you want information about.
newDescribeLocationEfs ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationEfs
newDescribeLocationEfs pLocationArn_ =
  DescribeLocationEfs' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the Amazon EFS file system location
-- that you want information about.
describeLocationEfs_locationArn :: Lens.Lens' DescribeLocationEfs Prelude.Text
describeLocationEfs_locationArn = Lens.lens (\DescribeLocationEfs' {locationArn} -> locationArn) (\s@DescribeLocationEfs' {} a -> s {locationArn = a} :: DescribeLocationEfs)

instance Core.AWSRequest DescribeLocationEfs where
  type
    AWSResponse DescribeLocationEfs =
      DescribeLocationEfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationEfsResponse'
            Prelude.<$> (x Data..?> "InTransitEncryption")
            Prelude.<*> (x Data..?> "AccessPointArn")
            Prelude.<*> (x Data..?> "FileSystemAccessRoleArn")
            Prelude.<*> (x Data..?> "LocationArn")
            Prelude.<*> (x Data..?> "Ec2Config")
            Prelude.<*> (x Data..?> "LocationUri")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationEfs where
  hashWithSalt _salt DescribeLocationEfs' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationEfs where
  rnf DescribeLocationEfs' {..} =
    Prelude.rnf locationArn

instance Data.ToHeaders DescribeLocationEfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeLocationEfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLocationEfs where
  toJSON DescribeLocationEfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Data..= locationArn)]
      )

instance Data.ToPath DescribeLocationEfs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocationEfs where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationEfsResponse
--
-- /See:/ 'newDescribeLocationEfsResponse' smart constructor.
data DescribeLocationEfsResponse = DescribeLocationEfsResponse'
  { -- | Describes whether DataSync uses Transport Layer Security (TLS)
    -- encryption when copying data to or from the Amazon EFS file system.
    inTransitEncryption :: Prelude.Maybe EfsInTransitEncryption,
    -- | The ARN of the access point that DataSync uses to access the Amazon EFS
    -- file system.
    accessPointArn :: Prelude.Maybe Prelude.Text,
    -- | The Identity and Access Management (IAM) role that DataSync assumes when
    -- mounting the Amazon EFS file system.
    fileSystemAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon EFS file system location.
    locationArn :: Prelude.Maybe Prelude.Text,
    ec2Config :: Prelude.Maybe Ec2Config,
    -- | The URL of the Amazon EFS file system location.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The time that the location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationEfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inTransitEncryption', 'describeLocationEfsResponse_inTransitEncryption' - Describes whether DataSync uses Transport Layer Security (TLS)
-- encryption when copying data to or from the Amazon EFS file system.
--
-- 'accessPointArn', 'describeLocationEfsResponse_accessPointArn' - The ARN of the access point that DataSync uses to access the Amazon EFS
-- file system.
--
-- 'fileSystemAccessRoleArn', 'describeLocationEfsResponse_fileSystemAccessRoleArn' - The Identity and Access Management (IAM) role that DataSync assumes when
-- mounting the Amazon EFS file system.
--
-- 'locationArn', 'describeLocationEfsResponse_locationArn' - The ARN of the Amazon EFS file system location.
--
-- 'ec2Config', 'describeLocationEfsResponse_ec2Config' - Undocumented member.
--
-- 'locationUri', 'describeLocationEfsResponse_locationUri' - The URL of the Amazon EFS file system location.
--
-- 'creationTime', 'describeLocationEfsResponse_creationTime' - The time that the location was created.
--
-- 'httpStatus', 'describeLocationEfsResponse_httpStatus' - The response's http status code.
newDescribeLocationEfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationEfsResponse
newDescribeLocationEfsResponse pHttpStatus_ =
  DescribeLocationEfsResponse'
    { inTransitEncryption =
        Prelude.Nothing,
      accessPointArn = Prelude.Nothing,
      fileSystemAccessRoleArn = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      ec2Config = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes whether DataSync uses Transport Layer Security (TLS)
-- encryption when copying data to or from the Amazon EFS file system.
describeLocationEfsResponse_inTransitEncryption :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe EfsInTransitEncryption)
describeLocationEfsResponse_inTransitEncryption = Lens.lens (\DescribeLocationEfsResponse' {inTransitEncryption} -> inTransitEncryption) (\s@DescribeLocationEfsResponse' {} a -> s {inTransitEncryption = a} :: DescribeLocationEfsResponse)

-- | The ARN of the access point that DataSync uses to access the Amazon EFS
-- file system.
describeLocationEfsResponse_accessPointArn :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_accessPointArn = Lens.lens (\DescribeLocationEfsResponse' {accessPointArn} -> accessPointArn) (\s@DescribeLocationEfsResponse' {} a -> s {accessPointArn = a} :: DescribeLocationEfsResponse)

-- | The Identity and Access Management (IAM) role that DataSync assumes when
-- mounting the Amazon EFS file system.
describeLocationEfsResponse_fileSystemAccessRoleArn :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_fileSystemAccessRoleArn = Lens.lens (\DescribeLocationEfsResponse' {fileSystemAccessRoleArn} -> fileSystemAccessRoleArn) (\s@DescribeLocationEfsResponse' {} a -> s {fileSystemAccessRoleArn = a} :: DescribeLocationEfsResponse)

-- | The ARN of the Amazon EFS file system location.
describeLocationEfsResponse_locationArn :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_locationArn = Lens.lens (\DescribeLocationEfsResponse' {locationArn} -> locationArn) (\s@DescribeLocationEfsResponse' {} a -> s {locationArn = a} :: DescribeLocationEfsResponse)

-- | Undocumented member.
describeLocationEfsResponse_ec2Config :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Ec2Config)
describeLocationEfsResponse_ec2Config = Lens.lens (\DescribeLocationEfsResponse' {ec2Config} -> ec2Config) (\s@DescribeLocationEfsResponse' {} a -> s {ec2Config = a} :: DescribeLocationEfsResponse)

-- | The URL of the Amazon EFS file system location.
describeLocationEfsResponse_locationUri :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_locationUri = Lens.lens (\DescribeLocationEfsResponse' {locationUri} -> locationUri) (\s@DescribeLocationEfsResponse' {} a -> s {locationUri = a} :: DescribeLocationEfsResponse)

-- | The time that the location was created.
describeLocationEfsResponse_creationTime :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationEfsResponse_creationTime = Lens.lens (\DescribeLocationEfsResponse' {creationTime} -> creationTime) (\s@DescribeLocationEfsResponse' {} a -> s {creationTime = a} :: DescribeLocationEfsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeLocationEfsResponse_httpStatus :: Lens.Lens' DescribeLocationEfsResponse Prelude.Int
describeLocationEfsResponse_httpStatus = Lens.lens (\DescribeLocationEfsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationEfsResponse' {} a -> s {httpStatus = a} :: DescribeLocationEfsResponse)

instance Prelude.NFData DescribeLocationEfsResponse where
  rnf DescribeLocationEfsResponse' {..} =
    Prelude.rnf inTransitEncryption
      `Prelude.seq` Prelude.rnf accessPointArn
      `Prelude.seq` Prelude.rnf fileSystemAccessRoleArn
      `Prelude.seq` Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf ec2Config
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
