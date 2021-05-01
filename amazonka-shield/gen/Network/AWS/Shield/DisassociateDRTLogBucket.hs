{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.DisassociateDRTLogBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team\'s (DRT) access to the specified Amazon
-- S3 bucket containing your AWS WAF logs.
--
-- To make a @DisassociateDRTLogBucket@ request, you must be subscribed to
-- the
-- <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan>
-- or the
-- <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan>.
-- However, if you are not subscribed to one of these support plans, but
-- had been previously and had granted the DRT access to your account, you
-- can submit a @DisassociateDRTLogBucket@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTLogBucket
  ( -- * Creating a Request
    DisassociateDRTLogBucket (..),
    newDisassociateDRTLogBucket,

    -- * Request Lenses
    disassociateDRTLogBucket_logBucket,

    -- * Destructuring the Response
    DisassociateDRTLogBucketResponse (..),
    newDisassociateDRTLogBucketResponse,

    -- * Response Lenses
    disassociateDRTLogBucketResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisassociateDRTLogBucket' smart constructor.
data DisassociateDRTLogBucket = DisassociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDRTLogBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logBucket', 'disassociateDRTLogBucket_logBucket' - The Amazon S3 bucket that contains your AWS WAF logs.
newDisassociateDRTLogBucket ::
  -- | 'logBucket'
  Prelude.Text ->
  DisassociateDRTLogBucket
newDisassociateDRTLogBucket pLogBucket_ =
  DisassociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
disassociateDRTLogBucket_logBucket :: Lens.Lens' DisassociateDRTLogBucket Prelude.Text
disassociateDRTLogBucket_logBucket = Lens.lens (\DisassociateDRTLogBucket' {logBucket} -> logBucket) (\s@DisassociateDRTLogBucket' {} a -> s {logBucket = a} :: DisassociateDRTLogBucket)

instance Prelude.AWSRequest DisassociateDRTLogBucket where
  type
    Rs DisassociateDRTLogBucket =
      DisassociateDRTLogBucketResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDRTLogBucketResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDRTLogBucket

instance Prelude.NFData DisassociateDRTLogBucket

instance Prelude.ToHeaders DisassociateDRTLogBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.DisassociateDRTLogBucket" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateDRTLogBucket where
  toJSON DisassociateDRTLogBucket' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("LogBucket" Prelude..= logBucket)]
      )

instance Prelude.ToPath DisassociateDRTLogBucket where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateDRTLogBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDRTLogBucketResponse' smart constructor.
data DisassociateDRTLogBucketResponse = DisassociateDRTLogBucketResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDRTLogBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDRTLogBucketResponse_httpStatus' - The response's http status code.
newDisassociateDRTLogBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDRTLogBucketResponse
newDisassociateDRTLogBucketResponse pHttpStatus_ =
  DisassociateDRTLogBucketResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDRTLogBucketResponse_httpStatus :: Lens.Lens' DisassociateDRTLogBucketResponse Prelude.Int
disassociateDRTLogBucketResponse_httpStatus = Lens.lens (\DisassociateDRTLogBucketResponse' {httpStatus} -> httpStatus) (\s@DisassociateDRTLogBucketResponse' {} a -> s {httpStatus = a} :: DisassociateDRTLogBucketResponse)

instance
  Prelude.NFData
    DisassociateDRTLogBucketResponse
