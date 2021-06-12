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
-- Module      : Network.AWS.Shield.DescribeDRTAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current role and list of Amazon S3 log buckets used by the
-- DDoS Response Team (DRT) to access your AWS account while assisting with
-- attack mitigation.
module Network.AWS.Shield.DescribeDRTAccess
  ( -- * Creating a Request
    DescribeDRTAccess (..),
    newDescribeDRTAccess,

    -- * Destructuring the Response
    DescribeDRTAccessResponse (..),
    newDescribeDRTAccessResponse,

    -- * Response Lenses
    describeDRTAccessResponse_roleArn,
    describeDRTAccessResponse_logBucketList,
    describeDRTAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeDRTAccess' smart constructor.
data DescribeDRTAccess = DescribeDRTAccess'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDRTAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDRTAccess ::
  DescribeDRTAccess
newDescribeDRTAccess = DescribeDRTAccess'

instance Core.AWSRequest DescribeDRTAccess where
  type
    AWSResponse DescribeDRTAccess =
      DescribeDRTAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDRTAccessResponse'
            Core.<$> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "LogBucketList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDRTAccess

instance Core.NFData DescribeDRTAccess

instance Core.ToHeaders DescribeDRTAccess where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeDRTAccess" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDRTAccess where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeDRTAccess where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDRTAccess where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDRTAccessResponse' smart constructor.
data DescribeDRTAccessResponse = DescribeDRTAccessResponse'
  { -- | The Amazon Resource Name (ARN) of the role the DRT used to access your
    -- AWS account.
    roleArn :: Core.Maybe Core.Text,
    -- | The list of Amazon S3 buckets accessed by the DRT.
    logBucketList :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDRTAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'describeDRTAccessResponse_roleArn' - The Amazon Resource Name (ARN) of the role the DRT used to access your
-- AWS account.
--
-- 'logBucketList', 'describeDRTAccessResponse_logBucketList' - The list of Amazon S3 buckets accessed by the DRT.
--
-- 'httpStatus', 'describeDRTAccessResponse_httpStatus' - The response's http status code.
newDescribeDRTAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDRTAccessResponse
newDescribeDRTAccessResponse pHttpStatus_ =
  DescribeDRTAccessResponse'
    { roleArn = Core.Nothing,
      logBucketList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the role the DRT used to access your
-- AWS account.
describeDRTAccessResponse_roleArn :: Lens.Lens' DescribeDRTAccessResponse (Core.Maybe Core.Text)
describeDRTAccessResponse_roleArn = Lens.lens (\DescribeDRTAccessResponse' {roleArn} -> roleArn) (\s@DescribeDRTAccessResponse' {} a -> s {roleArn = a} :: DescribeDRTAccessResponse)

-- | The list of Amazon S3 buckets accessed by the DRT.
describeDRTAccessResponse_logBucketList :: Lens.Lens' DescribeDRTAccessResponse (Core.Maybe [Core.Text])
describeDRTAccessResponse_logBucketList = Lens.lens (\DescribeDRTAccessResponse' {logBucketList} -> logBucketList) (\s@DescribeDRTAccessResponse' {} a -> s {logBucketList = a} :: DescribeDRTAccessResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDRTAccessResponse_httpStatus :: Lens.Lens' DescribeDRTAccessResponse Core.Int
describeDRTAccessResponse_httpStatus = Lens.lens (\DescribeDRTAccessResponse' {httpStatus} -> httpStatus) (\s@DescribeDRTAccessResponse' {} a -> s {httpStatus = a} :: DescribeDRTAccessResponse)

instance Core.NFData DescribeDRTAccessResponse
