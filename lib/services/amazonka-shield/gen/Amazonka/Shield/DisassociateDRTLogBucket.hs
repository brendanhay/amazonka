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
-- Module      : Amazonka.Shield.DisassociateDRTLogBucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the Shield Response Team\'s (SRT) access to the specified Amazon
-- S3 bucket containing the logs that you shared previously.
module Amazonka.Shield.DisassociateDRTLogBucket
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDisassociateDRTLogBucket' smart constructor.
data DisassociateDRTLogBucket = DisassociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains the logs that you want to share.
    logBucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDRTLogBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logBucket', 'disassociateDRTLogBucket_logBucket' - The Amazon S3 bucket that contains the logs that you want to share.
newDisassociateDRTLogBucket ::
  -- | 'logBucket'
  Prelude.Text ->
  DisassociateDRTLogBucket
newDisassociateDRTLogBucket pLogBucket_ =
  DisassociateDRTLogBucket' {logBucket = pLogBucket_}

-- | The Amazon S3 bucket that contains the logs that you want to share.
disassociateDRTLogBucket_logBucket :: Lens.Lens' DisassociateDRTLogBucket Prelude.Text
disassociateDRTLogBucket_logBucket = Lens.lens (\DisassociateDRTLogBucket' {logBucket} -> logBucket) (\s@DisassociateDRTLogBucket' {} a -> s {logBucket = a} :: DisassociateDRTLogBucket)

instance Core.AWSRequest DisassociateDRTLogBucket where
  type
    AWSResponse DisassociateDRTLogBucket =
      DisassociateDRTLogBucketResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDRTLogBucketResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDRTLogBucket where
  hashWithSalt _salt DisassociateDRTLogBucket' {..} =
    _salt `Prelude.hashWithSalt` logBucket

instance Prelude.NFData DisassociateDRTLogBucket where
  rnf DisassociateDRTLogBucket' {..} =
    Prelude.rnf logBucket

instance Data.ToHeaders DisassociateDRTLogBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DisassociateDRTLogBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDRTLogBucket where
  toJSON DisassociateDRTLogBucket' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LogBucket" Data..= logBucket)]
      )

instance Data.ToPath DisassociateDRTLogBucket where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateDRTLogBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDRTLogBucketResponse' smart constructor.
data DisassociateDRTLogBucketResponse = DisassociateDRTLogBucketResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateDRTLogBucketResponse' {..} =
    Prelude.rnf httpStatus
