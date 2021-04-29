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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeDRTAccess' smart constructor.
data DescribeDRTAccess = DescribeDRTAccess'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeDRTAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDRTAccess ::
  DescribeDRTAccess
newDescribeDRTAccess = DescribeDRTAccess'

instance Prelude.AWSRequest DescribeDRTAccess where
  type Rs DescribeDRTAccess = DescribeDRTAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDRTAccessResponse'
            Prelude.<$> (x Prelude..?> "RoleArn")
            Prelude.<*> ( x Prelude..?> "LogBucketList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDRTAccess

instance Prelude.NFData DescribeDRTAccess

instance Prelude.ToHeaders DescribeDRTAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.DescribeDRTAccess" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeDRTAccess where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DescribeDRTAccess where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDRTAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDRTAccessResponse' smart constructor.
data DescribeDRTAccessResponse = DescribeDRTAccessResponse'
  { -- | The Amazon Resource Name (ARN) of the role the DRT used to access your
    -- AWS account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The list of Amazon S3 buckets accessed by the DRT.
    logBucketList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDRTAccessResponse
newDescribeDRTAccessResponse pHttpStatus_ =
  DescribeDRTAccessResponse'
    { roleArn =
        Prelude.Nothing,
      logBucketList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the role the DRT used to access your
-- AWS account.
describeDRTAccessResponse_roleArn :: Lens.Lens' DescribeDRTAccessResponse (Prelude.Maybe Prelude.Text)
describeDRTAccessResponse_roleArn = Lens.lens (\DescribeDRTAccessResponse' {roleArn} -> roleArn) (\s@DescribeDRTAccessResponse' {} a -> s {roleArn = a} :: DescribeDRTAccessResponse)

-- | The list of Amazon S3 buckets accessed by the DRT.
describeDRTAccessResponse_logBucketList :: Lens.Lens' DescribeDRTAccessResponse (Prelude.Maybe [Prelude.Text])
describeDRTAccessResponse_logBucketList = Lens.lens (\DescribeDRTAccessResponse' {logBucketList} -> logBucketList) (\s@DescribeDRTAccessResponse' {} a -> s {logBucketList = a} :: DescribeDRTAccessResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeDRTAccessResponse_httpStatus :: Lens.Lens' DescribeDRTAccessResponse Prelude.Int
describeDRTAccessResponse_httpStatus = Lens.lens (\DescribeDRTAccessResponse' {httpStatus} -> httpStatus) (\s@DescribeDRTAccessResponse' {} a -> s {httpStatus = a} :: DescribeDRTAccessResponse)

instance Prelude.NFData DescribeDRTAccessResponse
