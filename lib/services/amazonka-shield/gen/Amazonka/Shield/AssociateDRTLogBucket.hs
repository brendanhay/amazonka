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
-- Module      : Amazonka.Shield.AssociateDRTLogBucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the Shield Response Team (SRT) to access the specified Amazon
-- S3 bucket containing log data such as Application Load Balancer access
-- logs, CloudFront logs, or logs from third party sources. You can
-- associate up to 10 Amazon S3 buckets with your subscription.
--
-- To use the services of the SRT and make an @AssociateDRTLogBucket@
-- request, you must be subscribed to the
-- <http://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <http://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
module Amazonka.Shield.AssociateDRTLogBucket
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newAssociateDRTLogBucket' smart constructor.
data AssociateDRTLogBucket = AssociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains the logs that you want to share.
    logBucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDRTLogBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logBucket', 'associateDRTLogBucket_logBucket' - The Amazon S3 bucket that contains the logs that you want to share.
newAssociateDRTLogBucket ::
  -- | 'logBucket'
  Prelude.Text ->
  AssociateDRTLogBucket
newAssociateDRTLogBucket pLogBucket_ =
  AssociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains the logs that you want to share.
associateDRTLogBucket_logBucket :: Lens.Lens' AssociateDRTLogBucket Prelude.Text
associateDRTLogBucket_logBucket = Lens.lens (\AssociateDRTLogBucket' {logBucket} -> logBucket) (\s@AssociateDRTLogBucket' {} a -> s {logBucket = a} :: AssociateDRTLogBucket)

instance Core.AWSRequest AssociateDRTLogBucket where
  type
    AWSResponse AssociateDRTLogBucket =
      AssociateDRTLogBucketResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDRTLogBucketResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDRTLogBucket where
  hashWithSalt _salt AssociateDRTLogBucket' {..} =
    _salt `Prelude.hashWithSalt` logBucket

instance Prelude.NFData AssociateDRTLogBucket where
  rnf AssociateDRTLogBucket' {..} =
    Prelude.rnf logBucket

instance Data.ToHeaders AssociateDRTLogBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.AssociateDRTLogBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateDRTLogBucket where
  toJSON AssociateDRTLogBucket' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LogBucket" Data..= logBucket)]
      )

instance Data.ToPath AssociateDRTLogBucket where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateDRTLogBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDRTLogBucketResponse' smart constructor.
data AssociateDRTLogBucketResponse = AssociateDRTLogBucketResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateDRTLogBucketResponse
newAssociateDRTLogBucketResponse pHttpStatus_ =
  AssociateDRTLogBucketResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDRTLogBucketResponse_httpStatus :: Lens.Lens' AssociateDRTLogBucketResponse Prelude.Int
associateDRTLogBucketResponse_httpStatus = Lens.lens (\AssociateDRTLogBucketResponse' {httpStatus} -> httpStatus) (\s@AssociateDRTLogBucketResponse' {} a -> s {httpStatus = a} :: AssociateDRTLogBucketResponse)

instance Prelude.NFData AssociateDRTLogBucketResponse where
  rnf AssociateDRTLogBucketResponse' {..} =
    Prelude.rnf httpStatus
