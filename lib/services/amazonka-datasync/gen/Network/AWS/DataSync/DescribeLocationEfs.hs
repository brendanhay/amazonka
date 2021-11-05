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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the path information about an Amazon EFS
-- location.
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
    describeLocationEfsResponse_creationTime,
    describeLocationEfsResponse_locationUri,
    describeLocationEfsResponse_locationArn,
    describeLocationEfsResponse_ec2Config,
    describeLocationEfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataSync.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationEfsRequest
--
-- /See:/ 'newDescribeLocationEfs' smart constructor.
data DescribeLocationEfs = DescribeLocationEfs'
  { -- | The Amazon Resource Name (ARN) of the EFS location to describe.
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
-- 'locationArn', 'describeLocationEfs_locationArn' - The Amazon Resource Name (ARN) of the EFS location to describe.
newDescribeLocationEfs ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationEfs
newDescribeLocationEfs pLocationArn_ =
  DescribeLocationEfs' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the EFS location to describe.
describeLocationEfs_locationArn :: Lens.Lens' DescribeLocationEfs Prelude.Text
describeLocationEfs_locationArn = Lens.lens (\DescribeLocationEfs' {locationArn} -> locationArn) (\s@DescribeLocationEfs' {} a -> s {locationArn = a} :: DescribeLocationEfs)

instance Core.AWSRequest DescribeLocationEfs where
  type
    AWSResponse DescribeLocationEfs =
      DescribeLocationEfsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationEfsResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "Ec2Config")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationEfs

instance Prelude.NFData DescribeLocationEfs

instance Core.ToHeaders DescribeLocationEfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.DescribeLocationEfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLocationEfs where
  toJSON DescribeLocationEfs' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DescribeLocationEfs where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocationEfs where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationEfsResponse
--
-- /See:/ 'newDescribeLocationEfsResponse' smart constructor.
data DescribeLocationEfsResponse = DescribeLocationEfsResponse'
  { -- | The time that the EFS location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The URL of the EFS location that was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the EFS location that was described.
    locationArn :: Prelude.Maybe Prelude.Text,
    ec2Config :: Prelude.Maybe Ec2Config,
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
-- 'creationTime', 'describeLocationEfsResponse_creationTime' - The time that the EFS location was created.
--
-- 'locationUri', 'describeLocationEfsResponse_locationUri' - The URL of the EFS location that was described.
--
-- 'locationArn', 'describeLocationEfsResponse_locationArn' - The Amazon Resource Name (ARN) of the EFS location that was described.
--
-- 'ec2Config', 'describeLocationEfsResponse_ec2Config' - Undocumented member.
--
-- 'httpStatus', 'describeLocationEfsResponse_httpStatus' - The response's http status code.
newDescribeLocationEfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationEfsResponse
newDescribeLocationEfsResponse pHttpStatus_ =
  DescribeLocationEfsResponse'
    { creationTime =
        Prelude.Nothing,
      locationUri = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      ec2Config = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time that the EFS location was created.
describeLocationEfsResponse_creationTime :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationEfsResponse_creationTime = Lens.lens (\DescribeLocationEfsResponse' {creationTime} -> creationTime) (\s@DescribeLocationEfsResponse' {} a -> s {creationTime = a} :: DescribeLocationEfsResponse) Prelude.. Lens.mapping Core._Time

-- | The URL of the EFS location that was described.
describeLocationEfsResponse_locationUri :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_locationUri = Lens.lens (\DescribeLocationEfsResponse' {locationUri} -> locationUri) (\s@DescribeLocationEfsResponse' {} a -> s {locationUri = a} :: DescribeLocationEfsResponse)

-- | The Amazon Resource Name (ARN) of the EFS location that was described.
describeLocationEfsResponse_locationArn :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Prelude.Text)
describeLocationEfsResponse_locationArn = Lens.lens (\DescribeLocationEfsResponse' {locationArn} -> locationArn) (\s@DescribeLocationEfsResponse' {} a -> s {locationArn = a} :: DescribeLocationEfsResponse)

-- | Undocumented member.
describeLocationEfsResponse_ec2Config :: Lens.Lens' DescribeLocationEfsResponse (Prelude.Maybe Ec2Config)
describeLocationEfsResponse_ec2Config = Lens.lens (\DescribeLocationEfsResponse' {ec2Config} -> ec2Config) (\s@DescribeLocationEfsResponse' {} a -> s {ec2Config = a} :: DescribeLocationEfsResponse)

-- | The response's http status code.
describeLocationEfsResponse_httpStatus :: Lens.Lens' DescribeLocationEfsResponse Prelude.Int
describeLocationEfsResponse_httpStatus = Lens.lens (\DescribeLocationEfsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationEfsResponse' {} a -> s {httpStatus = a} :: DescribeLocationEfsResponse)

instance Prelude.NFData DescribeLocationEfsResponse
