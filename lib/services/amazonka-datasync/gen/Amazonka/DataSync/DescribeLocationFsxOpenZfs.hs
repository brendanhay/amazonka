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
-- Module      : Amazonka.DataSync.DescribeLocationFsxOpenZfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about how an DataSync location for an Amazon FSx for
-- OpenZFS file system is configured.
--
-- Response elements related to @SMB@ aren\'t supported with the
-- @DescribeLocationFsxOpenZfs@ operation.
module Amazonka.DataSync.DescribeLocationFsxOpenZfs
  ( -- * Creating a Request
    DescribeLocationFsxOpenZfs (..),
    newDescribeLocationFsxOpenZfs,

    -- * Request Lenses
    describeLocationFsxOpenZfs_locationArn,

    -- * Destructuring the Response
    DescribeLocationFsxOpenZfsResponse (..),
    newDescribeLocationFsxOpenZfsResponse,

    -- * Response Lenses
    describeLocationFsxOpenZfsResponse_locationArn,
    describeLocationFsxOpenZfsResponse_locationUri,
    describeLocationFsxOpenZfsResponse_securityGroupArns,
    describeLocationFsxOpenZfsResponse_creationTime,
    describeLocationFsxOpenZfsResponse_protocol,
    describeLocationFsxOpenZfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocationFsxOpenZfs' smart constructor.
data DescribeLocationFsxOpenZfs = DescribeLocationFsxOpenZfs'
  { -- | The Amazon Resource Name (ARN) of the FSx for OpenZFS location to
    -- describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationFsxOpenZfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationFsxOpenZfs_locationArn' - The Amazon Resource Name (ARN) of the FSx for OpenZFS location to
-- describe.
newDescribeLocationFsxOpenZfs ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationFsxOpenZfs
newDescribeLocationFsxOpenZfs pLocationArn_ =
  DescribeLocationFsxOpenZfs'
    { locationArn =
        pLocationArn_
    }

-- | The Amazon Resource Name (ARN) of the FSx for OpenZFS location to
-- describe.
describeLocationFsxOpenZfs_locationArn :: Lens.Lens' DescribeLocationFsxOpenZfs Prelude.Text
describeLocationFsxOpenZfs_locationArn = Lens.lens (\DescribeLocationFsxOpenZfs' {locationArn} -> locationArn) (\s@DescribeLocationFsxOpenZfs' {} a -> s {locationArn = a} :: DescribeLocationFsxOpenZfs)

instance Core.AWSRequest DescribeLocationFsxOpenZfs where
  type
    AWSResponse DescribeLocationFsxOpenZfs =
      DescribeLocationFsxOpenZfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationFsxOpenZfsResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (x Data..?> "LocationUri")
            Prelude.<*> (x Data..?> "SecurityGroupArns")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Protocol")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationFsxOpenZfs where
  hashWithSalt _salt DescribeLocationFsxOpenZfs' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationFsxOpenZfs where
  rnf DescribeLocationFsxOpenZfs' {..} =
    Prelude.rnf locationArn

instance Data.ToHeaders DescribeLocationFsxOpenZfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeLocationFsxOpenZfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLocationFsxOpenZfs where
  toJSON DescribeLocationFsxOpenZfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Data..= locationArn)]
      )

instance Data.ToPath DescribeLocationFsxOpenZfs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocationFsxOpenZfs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLocationFsxOpenZfsResponse' smart constructor.
data DescribeLocationFsxOpenZfsResponse = DescribeLocationFsxOpenZfsResponse'
  { -- | The ARN of the FSx for OpenZFS location that was described.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The uniform resource identifier (URI) of the FSx for OpenZFS location
    -- that was described.
    --
    -- Example:
    -- @fsxz:\/\/us-west-2.fs-1234567890abcdef02\/fsx\/folderA\/folder@
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the security groups that are configured for the FSx for
    -- OpenZFS file system.
    securityGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The time that the FSx for OpenZFS location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The type of protocol that DataSync uses to access your file system.
    protocol :: Prelude.Maybe FsxProtocol,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationFsxOpenZfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationFsxOpenZfsResponse_locationArn' - The ARN of the FSx for OpenZFS location that was described.
--
-- 'locationUri', 'describeLocationFsxOpenZfsResponse_locationUri' - The uniform resource identifier (URI) of the FSx for OpenZFS location
-- that was described.
--
-- Example:
-- @fsxz:\/\/us-west-2.fs-1234567890abcdef02\/fsx\/folderA\/folder@
--
-- 'securityGroupArns', 'describeLocationFsxOpenZfsResponse_securityGroupArns' - The ARNs of the security groups that are configured for the FSx for
-- OpenZFS file system.
--
-- 'creationTime', 'describeLocationFsxOpenZfsResponse_creationTime' - The time that the FSx for OpenZFS location was created.
--
-- 'protocol', 'describeLocationFsxOpenZfsResponse_protocol' - The type of protocol that DataSync uses to access your file system.
--
-- 'httpStatus', 'describeLocationFsxOpenZfsResponse_httpStatus' - The response's http status code.
newDescribeLocationFsxOpenZfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationFsxOpenZfsResponse
newDescribeLocationFsxOpenZfsResponse pHttpStatus_ =
  DescribeLocationFsxOpenZfsResponse'
    { locationArn =
        Prelude.Nothing,
      locationUri = Prelude.Nothing,
      securityGroupArns = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      protocol = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the FSx for OpenZFS location that was described.
describeLocationFsxOpenZfsResponse_locationArn :: Lens.Lens' DescribeLocationFsxOpenZfsResponse (Prelude.Maybe Prelude.Text)
describeLocationFsxOpenZfsResponse_locationArn = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {locationArn} -> locationArn) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {locationArn = a} :: DescribeLocationFsxOpenZfsResponse)

-- | The uniform resource identifier (URI) of the FSx for OpenZFS location
-- that was described.
--
-- Example:
-- @fsxz:\/\/us-west-2.fs-1234567890abcdef02\/fsx\/folderA\/folder@
describeLocationFsxOpenZfsResponse_locationUri :: Lens.Lens' DescribeLocationFsxOpenZfsResponse (Prelude.Maybe Prelude.Text)
describeLocationFsxOpenZfsResponse_locationUri = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {locationUri} -> locationUri) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {locationUri = a} :: DescribeLocationFsxOpenZfsResponse)

-- | The ARNs of the security groups that are configured for the FSx for
-- OpenZFS file system.
describeLocationFsxOpenZfsResponse_securityGroupArns :: Lens.Lens' DescribeLocationFsxOpenZfsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationFsxOpenZfsResponse_securityGroupArns = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {securityGroupArns} -> securityGroupArns) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {securityGroupArns = a} :: DescribeLocationFsxOpenZfsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time that the FSx for OpenZFS location was created.
describeLocationFsxOpenZfsResponse_creationTime :: Lens.Lens' DescribeLocationFsxOpenZfsResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationFsxOpenZfsResponse_creationTime = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {creationTime} -> creationTime) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {creationTime = a} :: DescribeLocationFsxOpenZfsResponse) Prelude.. Lens.mapping Data._Time

-- | The type of protocol that DataSync uses to access your file system.
describeLocationFsxOpenZfsResponse_protocol :: Lens.Lens' DescribeLocationFsxOpenZfsResponse (Prelude.Maybe FsxProtocol)
describeLocationFsxOpenZfsResponse_protocol = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {protocol} -> protocol) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {protocol = a} :: DescribeLocationFsxOpenZfsResponse)

-- | The response's http status code.
describeLocationFsxOpenZfsResponse_httpStatus :: Lens.Lens' DescribeLocationFsxOpenZfsResponse Prelude.Int
describeLocationFsxOpenZfsResponse_httpStatus = Lens.lens (\DescribeLocationFsxOpenZfsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationFsxOpenZfsResponse' {} a -> s {httpStatus = a} :: DescribeLocationFsxOpenZfsResponse)

instance
  Prelude.NFData
    DescribeLocationFsxOpenZfsResponse
  where
  rnf DescribeLocationFsxOpenZfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf securityGroupArns
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf httpStatus
