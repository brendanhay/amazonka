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
-- Module      : Network.AWS.Shield.AssociateDRTLogBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to access the specified Amazon
-- S3 bucket containing your AWS WAF logs. You can associate up to 10
-- Amazon S3 buckets with your subscription.
--
-- To use the services of the DRT and make an @AssociateDRTLogBucket@
-- request, you must be subscribed to the
-- <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
module Network.AWS.Shield.AssociateDRTLogBucket
  ( -- * Creating a Request
    AssociateDRTLogBucket (..),
    newAssociateDRTLogBucket,

    -- * Request Lenses
    associateDRTLogBucket_logBucket,

    -- * Destructuring the Response
    AssociateDRTLogBucketResponse (..),
    newAssociateDRTLogBucketResponse,

    -- * Response Lenses
    associateDRTLogBucketResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newAssociateDRTLogBucket' smart constructor.
data AssociateDRTLogBucket = AssociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDRTLogBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logBucket', 'associateDRTLogBucket_logBucket' - The Amazon S3 bucket that contains your AWS WAF logs.
newAssociateDRTLogBucket ::
  -- | 'logBucket'
  Core.Text ->
  AssociateDRTLogBucket
newAssociateDRTLogBucket pLogBucket_ =
  AssociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
associateDRTLogBucket_logBucket :: Lens.Lens' AssociateDRTLogBucket Core.Text
associateDRTLogBucket_logBucket = Lens.lens (\AssociateDRTLogBucket' {logBucket} -> logBucket) (\s@AssociateDRTLogBucket' {} a -> s {logBucket = a} :: AssociateDRTLogBucket)

instance Core.AWSRequest AssociateDRTLogBucket where
  type
    AWSResponse AssociateDRTLogBucket =
      AssociateDRTLogBucketResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDRTLogBucketResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateDRTLogBucket

instance Core.NFData AssociateDRTLogBucket

instance Core.ToHeaders AssociateDRTLogBucket where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.AssociateDRTLogBucket" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateDRTLogBucket where
  toJSON AssociateDRTLogBucket' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("LogBucket" Core..= logBucket)]
      )

instance Core.ToPath AssociateDRTLogBucket where
  toPath = Core.const "/"

instance Core.ToQuery AssociateDRTLogBucket where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateDRTLogBucketResponse' smart constructor.
data AssociateDRTLogBucketResponse = AssociateDRTLogBucketResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDRTLogBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDRTLogBucketResponse_httpStatus' - The response's http status code.
newAssociateDRTLogBucketResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateDRTLogBucketResponse
newAssociateDRTLogBucketResponse pHttpStatus_ =
  AssociateDRTLogBucketResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDRTLogBucketResponse_httpStatus :: Lens.Lens' AssociateDRTLogBucketResponse Core.Int
associateDRTLogBucketResponse_httpStatus = Lens.lens (\AssociateDRTLogBucketResponse' {httpStatus} -> httpStatus) (\s@AssociateDRTLogBucketResponse' {} a -> s {httpStatus = a} :: AssociateDRTLogBucketResponse)

instance Core.NFData AssociateDRTLogBucketResponse
