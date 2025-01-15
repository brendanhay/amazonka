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
-- Module      : Amazonka.Backup.DescribeProtectedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a saved resource, including the last time it
-- was backed up, its Amazon Resource Name (ARN), and the Amazon Web
-- Services service type of the saved resource.
module Amazonka.Backup.DescribeProtectedResource
  ( -- * Creating a Request
    DescribeProtectedResource (..),
    newDescribeProtectedResource,

    -- * Request Lenses
    describeProtectedResource_resourceArn,

    -- * Destructuring the Response
    DescribeProtectedResourceResponse (..),
    newDescribeProtectedResourceResponse,

    -- * Response Lenses
    describeProtectedResourceResponse_lastBackupTime,
    describeProtectedResourceResponse_resourceArn,
    describeProtectedResourceResponse_resourceType,
    describeProtectedResourceResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProtectedResource' smart constructor.
data DescribeProtectedResource = DescribeProtectedResource'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtectedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'describeProtectedResource_resourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
newDescribeProtectedResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  DescribeProtectedResource
newDescribeProtectedResource pResourceArn_ =
  DescribeProtectedResource'
    { resourceArn =
        pResourceArn_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
describeProtectedResource_resourceArn :: Lens.Lens' DescribeProtectedResource Prelude.Text
describeProtectedResource_resourceArn = Lens.lens (\DescribeProtectedResource' {resourceArn} -> resourceArn) (\s@DescribeProtectedResource' {} a -> s {resourceArn = a} :: DescribeProtectedResource)

instance Core.AWSRequest DescribeProtectedResource where
  type
    AWSResponse DescribeProtectedResource =
      DescribeProtectedResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectedResourceResponse'
            Prelude.<$> (x Data..?> "LastBackupTime")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "ResourceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProtectedResource where
  hashWithSalt _salt DescribeProtectedResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DescribeProtectedResource where
  rnf DescribeProtectedResource' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders DescribeProtectedResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProtectedResource where
  toPath DescribeProtectedResource' {..} =
    Prelude.mconcat
      ["/resources/", Data.toBS resourceArn]

instance Data.ToQuery DescribeProtectedResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProtectedResourceResponse' smart constructor.
data DescribeProtectedResourceResponse = DescribeProtectedResourceResponse'
  { -- | The date and time that a resource was last backed up, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @LastBackupTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    lastBackupTime :: Prelude.Maybe Data.POSIX,
    -- | An ARN that uniquely identifies a resource. The format of the ARN
    -- depends on the resource type.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services resource saved as a recovery point; for
    -- example, an Amazon EBS volume or an Amazon RDS database.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtectedResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastBackupTime', 'describeProtectedResourceResponse_lastBackupTime' - The date and time that a resource was last backed up, in Unix format and
-- Coordinated Universal Time (UTC). The value of @LastBackupTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'resourceArn', 'describeProtectedResourceResponse_resourceArn' - An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
--
-- 'resourceType', 'describeProtectedResourceResponse_resourceType' - The type of Amazon Web Services resource saved as a recovery point; for
-- example, an Amazon EBS volume or an Amazon RDS database.
--
-- 'httpStatus', 'describeProtectedResourceResponse_httpStatus' - The response's http status code.
newDescribeProtectedResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProtectedResourceResponse
newDescribeProtectedResourceResponse pHttpStatus_ =
  DescribeProtectedResourceResponse'
    { lastBackupTime =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that a resource was last backed up, in Unix format and
-- Coordinated Universal Time (UTC). The value of @LastBackupTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeProtectedResourceResponse_lastBackupTime :: Lens.Lens' DescribeProtectedResourceResponse (Prelude.Maybe Prelude.UTCTime)
describeProtectedResourceResponse_lastBackupTime = Lens.lens (\DescribeProtectedResourceResponse' {lastBackupTime} -> lastBackupTime) (\s@DescribeProtectedResourceResponse' {} a -> s {lastBackupTime = a} :: DescribeProtectedResourceResponse) Prelude.. Lens.mapping Data._Time

-- | An ARN that uniquely identifies a resource. The format of the ARN
-- depends on the resource type.
describeProtectedResourceResponse_resourceArn :: Lens.Lens' DescribeProtectedResourceResponse (Prelude.Maybe Prelude.Text)
describeProtectedResourceResponse_resourceArn = Lens.lens (\DescribeProtectedResourceResponse' {resourceArn} -> resourceArn) (\s@DescribeProtectedResourceResponse' {} a -> s {resourceArn = a} :: DescribeProtectedResourceResponse)

-- | The type of Amazon Web Services resource saved as a recovery point; for
-- example, an Amazon EBS volume or an Amazon RDS database.
describeProtectedResourceResponse_resourceType :: Lens.Lens' DescribeProtectedResourceResponse (Prelude.Maybe Prelude.Text)
describeProtectedResourceResponse_resourceType = Lens.lens (\DescribeProtectedResourceResponse' {resourceType} -> resourceType) (\s@DescribeProtectedResourceResponse' {} a -> s {resourceType = a} :: DescribeProtectedResourceResponse)

-- | The response's http status code.
describeProtectedResourceResponse_httpStatus :: Lens.Lens' DescribeProtectedResourceResponse Prelude.Int
describeProtectedResourceResponse_httpStatus = Lens.lens (\DescribeProtectedResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeProtectedResourceResponse' {} a -> s {httpStatus = a} :: DescribeProtectedResourceResponse)

instance
  Prelude.NFData
    DescribeProtectedResourceResponse
  where
  rnf DescribeProtectedResourceResponse' {..} =
    Prelude.rnf lastBackupTime `Prelude.seq`
      Prelude.rnf resourceArn `Prelude.seq`
        Prelude.rnf resourceType `Prelude.seq`
          Prelude.rnf httpStatus
