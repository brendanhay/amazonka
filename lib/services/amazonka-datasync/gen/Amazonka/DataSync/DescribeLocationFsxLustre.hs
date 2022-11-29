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
-- Module      : Amazonka.DataSync.DescribeLocationFsxLustre
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about how an DataSync location for an Amazon FSx for
-- Lustre file system is configured.
module Amazonka.DataSync.DescribeLocationFsxLustre
  ( -- * Creating a Request
    DescribeLocationFsxLustre (..),
    newDescribeLocationFsxLustre,

    -- * Request Lenses
    describeLocationFsxLustre_locationArn,

    -- * Destructuring the Response
    DescribeLocationFsxLustreResponse (..),
    newDescribeLocationFsxLustreResponse,

    -- * Response Lenses
    describeLocationFsxLustreResponse_locationArn,
    describeLocationFsxLustreResponse_locationUri,
    describeLocationFsxLustreResponse_securityGroupArns,
    describeLocationFsxLustreResponse_creationTime,
    describeLocationFsxLustreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocationFsxLustre' smart constructor.
data DescribeLocationFsxLustre = DescribeLocationFsxLustre'
  { -- | The Amazon Resource Name (ARN) of the FSx for Lustre location to
    -- describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationFsxLustre' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationFsxLustre_locationArn' - The Amazon Resource Name (ARN) of the FSx for Lustre location to
-- describe.
newDescribeLocationFsxLustre ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationFsxLustre
newDescribeLocationFsxLustre pLocationArn_ =
  DescribeLocationFsxLustre'
    { locationArn =
        pLocationArn_
    }

-- | The Amazon Resource Name (ARN) of the FSx for Lustre location to
-- describe.
describeLocationFsxLustre_locationArn :: Lens.Lens' DescribeLocationFsxLustre Prelude.Text
describeLocationFsxLustre_locationArn = Lens.lens (\DescribeLocationFsxLustre' {locationArn} -> locationArn) (\s@DescribeLocationFsxLustre' {} a -> s {locationArn = a} :: DescribeLocationFsxLustre)

instance Core.AWSRequest DescribeLocationFsxLustre where
  type
    AWSResponse DescribeLocationFsxLustre =
      DescribeLocationFsxLustreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationFsxLustreResponse'
            Prelude.<$> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "SecurityGroupArns")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationFsxLustre where
  hashWithSalt _salt DescribeLocationFsxLustre' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationFsxLustre where
  rnf DescribeLocationFsxLustre' {..} =
    Prelude.rnf locationArn

instance Core.ToHeaders DescribeLocationFsxLustre where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.DescribeLocationFsxLustre" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLocationFsxLustre where
  toJSON DescribeLocationFsxLustre' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DescribeLocationFsxLustre where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocationFsxLustre where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLocationFsxLustreResponse' smart constructor.
data DescribeLocationFsxLustreResponse = DescribeLocationFsxLustreResponse'
  { -- | The Amazon Resource Name (ARN) of the FSx for Lustre location that was
    -- described.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The URI of the FSx for Lustre location that was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the security groups that are
    -- configured for the FSx for Lustre file system.
    securityGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The time that the FSx for Lustre location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationFsxLustreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationFsxLustreResponse_locationArn' - The Amazon Resource Name (ARN) of the FSx for Lustre location that was
-- described.
--
-- 'locationUri', 'describeLocationFsxLustreResponse_locationUri' - The URI of the FSx for Lustre location that was described.
--
-- 'securityGroupArns', 'describeLocationFsxLustreResponse_securityGroupArns' - The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the FSx for Lustre file system.
--
-- 'creationTime', 'describeLocationFsxLustreResponse_creationTime' - The time that the FSx for Lustre location was created.
--
-- 'httpStatus', 'describeLocationFsxLustreResponse_httpStatus' - The response's http status code.
newDescribeLocationFsxLustreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationFsxLustreResponse
newDescribeLocationFsxLustreResponse pHttpStatus_ =
  DescribeLocationFsxLustreResponse'
    { locationArn =
        Prelude.Nothing,
      locationUri = Prelude.Nothing,
      securityGroupArns = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the FSx for Lustre location that was
-- described.
describeLocationFsxLustreResponse_locationArn :: Lens.Lens' DescribeLocationFsxLustreResponse (Prelude.Maybe Prelude.Text)
describeLocationFsxLustreResponse_locationArn = Lens.lens (\DescribeLocationFsxLustreResponse' {locationArn} -> locationArn) (\s@DescribeLocationFsxLustreResponse' {} a -> s {locationArn = a} :: DescribeLocationFsxLustreResponse)

-- | The URI of the FSx for Lustre location that was described.
describeLocationFsxLustreResponse_locationUri :: Lens.Lens' DescribeLocationFsxLustreResponse (Prelude.Maybe Prelude.Text)
describeLocationFsxLustreResponse_locationUri = Lens.lens (\DescribeLocationFsxLustreResponse' {locationUri} -> locationUri) (\s@DescribeLocationFsxLustreResponse' {} a -> s {locationUri = a} :: DescribeLocationFsxLustreResponse)

-- | The Amazon Resource Names (ARNs) of the security groups that are
-- configured for the FSx for Lustre file system.
describeLocationFsxLustreResponse_securityGroupArns :: Lens.Lens' DescribeLocationFsxLustreResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationFsxLustreResponse_securityGroupArns = Lens.lens (\DescribeLocationFsxLustreResponse' {securityGroupArns} -> securityGroupArns) (\s@DescribeLocationFsxLustreResponse' {} a -> s {securityGroupArns = a} :: DescribeLocationFsxLustreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time that the FSx for Lustre location was created.
describeLocationFsxLustreResponse_creationTime :: Lens.Lens' DescribeLocationFsxLustreResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationFsxLustreResponse_creationTime = Lens.lens (\DescribeLocationFsxLustreResponse' {creationTime} -> creationTime) (\s@DescribeLocationFsxLustreResponse' {} a -> s {creationTime = a} :: DescribeLocationFsxLustreResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeLocationFsxLustreResponse_httpStatus :: Lens.Lens' DescribeLocationFsxLustreResponse Prelude.Int
describeLocationFsxLustreResponse_httpStatus = Lens.lens (\DescribeLocationFsxLustreResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationFsxLustreResponse' {} a -> s {httpStatus = a} :: DescribeLocationFsxLustreResponse)

instance
  Prelude.NFData
    DescribeLocationFsxLustreResponse
  where
  rnf DescribeLocationFsxLustreResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf securityGroupArns
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
