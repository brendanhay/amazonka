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
-- Module      : Network.AWS.AppStream.CreateUsageReportSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage report subscription. Usage reports are generated daily.
module Network.AWS.AppStream.CreateUsageReportSubscription
  ( -- * Creating a Request
    CreateUsageReportSubscription (..),
    newCreateUsageReportSubscription,

    -- * Destructuring the Response
    CreateUsageReportSubscriptionResponse (..),
    newCreateUsageReportSubscriptionResponse,

    -- * Response Lenses
    createUsageReportSubscriptionResponse_s3BucketName,
    createUsageReportSubscriptionResponse_schedule,
    createUsageReportSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUsageReportSubscription' smart constructor.
data CreateUsageReportSubscription = CreateUsageReportSubscription'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUsageReportSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateUsageReportSubscription ::
  CreateUsageReportSubscription
newCreateUsageReportSubscription =
  CreateUsageReportSubscription'

instance
  Core.AWSRequest
    CreateUsageReportSubscription
  where
  type
    AWSResponse CreateUsageReportSubscription =
      CreateUsageReportSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUsageReportSubscriptionResponse'
            Core.<$> (x Core..?> "S3BucketName")
            Core.<*> (x Core..?> "Schedule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUsageReportSubscription

instance Core.NFData CreateUsageReportSubscription

instance Core.ToHeaders CreateUsageReportSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateUsageReportSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUsageReportSubscription where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath CreateUsageReportSubscription where
  toPath = Core.const "/"

instance Core.ToQuery CreateUsageReportSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUsageReportSubscriptionResponse' smart constructor.
data CreateUsageReportSubscriptionResponse = CreateUsageReportSubscriptionResponse'
  { -- | The Amazon S3 bucket where generated reports are stored.
    --
    -- If you enabled on-instance session scripts and Amazon S3 logging for
    -- your session script configuration, AppStream 2.0 created an S3 bucket to
    -- store the script output. The bucket is unique to your account and
    -- Region. When you enable usage reporting in this case, AppStream 2.0 uses
    -- the same bucket to store your usage reports. If you haven\'t already
    -- enabled on-instance session scripts, when you enable usage reports,
    -- AppStream 2.0 creates a new S3 bucket.
    s3BucketName :: Core.Maybe Core.Text,
    -- | The schedule for generating usage reports.
    schedule :: Core.Maybe UsageReportSchedule,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUsageReportSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketName', 'createUsageReportSubscriptionResponse_s3BucketName' - The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for
-- your session script configuration, AppStream 2.0 created an S3 bucket to
-- store the script output. The bucket is unique to your account and
-- Region. When you enable usage reporting in this case, AppStream 2.0 uses
-- the same bucket to store your usage reports. If you haven\'t already
-- enabled on-instance session scripts, when you enable usage reports,
-- AppStream 2.0 creates a new S3 bucket.
--
-- 'schedule', 'createUsageReportSubscriptionResponse_schedule' - The schedule for generating usage reports.
--
-- 'httpStatus', 'createUsageReportSubscriptionResponse_httpStatus' - The response's http status code.
newCreateUsageReportSubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUsageReportSubscriptionResponse
newCreateUsageReportSubscriptionResponse pHttpStatus_ =
  CreateUsageReportSubscriptionResponse'
    { s3BucketName =
        Core.Nothing,
      schedule = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for
-- your session script configuration, AppStream 2.0 created an S3 bucket to
-- store the script output. The bucket is unique to your account and
-- Region. When you enable usage reporting in this case, AppStream 2.0 uses
-- the same bucket to store your usage reports. If you haven\'t already
-- enabled on-instance session scripts, when you enable usage reports,
-- AppStream 2.0 creates a new S3 bucket.
createUsageReportSubscriptionResponse_s3BucketName :: Lens.Lens' CreateUsageReportSubscriptionResponse (Core.Maybe Core.Text)
createUsageReportSubscriptionResponse_s3BucketName = Lens.lens (\CreateUsageReportSubscriptionResponse' {s3BucketName} -> s3BucketName) (\s@CreateUsageReportSubscriptionResponse' {} a -> s {s3BucketName = a} :: CreateUsageReportSubscriptionResponse)

-- | The schedule for generating usage reports.
createUsageReportSubscriptionResponse_schedule :: Lens.Lens' CreateUsageReportSubscriptionResponse (Core.Maybe UsageReportSchedule)
createUsageReportSubscriptionResponse_schedule = Lens.lens (\CreateUsageReportSubscriptionResponse' {schedule} -> schedule) (\s@CreateUsageReportSubscriptionResponse' {} a -> s {schedule = a} :: CreateUsageReportSubscriptionResponse)

-- | The response's http status code.
createUsageReportSubscriptionResponse_httpStatus :: Lens.Lens' CreateUsageReportSubscriptionResponse Core.Int
createUsageReportSubscriptionResponse_httpStatus = Lens.lens (\CreateUsageReportSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateUsageReportSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateUsageReportSubscriptionResponse)

instance
  Core.NFData
    CreateUsageReportSubscriptionResponse
