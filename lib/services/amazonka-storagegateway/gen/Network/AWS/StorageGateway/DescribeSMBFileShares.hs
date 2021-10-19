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
-- Module      : Network.AWS.StorageGateway.DescribeSMBFileShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Server Message Block (SMB) file
-- shares from a S3 File Gateway. This operation is only supported for S3
-- File Gateways.
module Network.AWS.StorageGateway.DescribeSMBFileShares
  ( -- * Creating a Request
    DescribeSMBFileShares (..),
    newDescribeSMBFileShares,

    -- * Request Lenses
    describeSMBFileShares_fileShareARNList,

    -- * Destructuring the Response
    DescribeSMBFileSharesResponse (..),
    newDescribeSMBFileSharesResponse,

    -- * Response Lenses
    describeSMBFileSharesResponse_sMBFileShareInfoList,
    describeSMBFileSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeSMBFileSharesInput
--
-- /See:/ 'newDescribeSMBFileShares' smart constructor.
data DescribeSMBFileShares = DescribeSMBFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to
    -- be described.
    fileShareARNList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSMBFileShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARNList', 'describeSMBFileShares_fileShareARNList' - An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
newDescribeSMBFileShares ::
  -- | 'fileShareARNList'
  Prelude.NonEmpty Prelude.Text ->
  DescribeSMBFileShares
newDescribeSMBFileShares pFileShareARNList_ =
  DescribeSMBFileShares'
    { fileShareARNList =
        Lens.coerced Lens.# pFileShareARNList_
    }

-- | An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
describeSMBFileShares_fileShareARNList :: Lens.Lens' DescribeSMBFileShares (Prelude.NonEmpty Prelude.Text)
describeSMBFileShares_fileShareARNList = Lens.lens (\DescribeSMBFileShares' {fileShareARNList} -> fileShareARNList) (\s@DescribeSMBFileShares' {} a -> s {fileShareARNList = a} :: DescribeSMBFileShares) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeSMBFileShares where
  type
    AWSResponse DescribeSMBFileShares =
      DescribeSMBFileSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSMBFileSharesResponse'
            Prelude.<$> ( x Core..?> "SMBFileShareInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSMBFileShares

instance Prelude.NFData DescribeSMBFileShares

instance Core.ToHeaders DescribeSMBFileShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeSMBFileShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSMBFileShares where
  toJSON DescribeSMBFileShares' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FileShareARNList" Core..= fileShareARNList)
          ]
      )

instance Core.ToPath DescribeSMBFileShares where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSMBFileShares where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeSMBFileSharesOutput
--
-- /See:/ 'newDescribeSMBFileSharesResponse' smart constructor.
data DescribeSMBFileSharesResponse = DescribeSMBFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    sMBFileShareInfoList :: Prelude.Maybe [SMBFileShareInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSMBFileSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMBFileShareInfoList', 'describeSMBFileSharesResponse_sMBFileShareInfoList' - An array containing a description for each requested file share.
--
-- 'httpStatus', 'describeSMBFileSharesResponse_httpStatus' - The response's http status code.
newDescribeSMBFileSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSMBFileSharesResponse
newDescribeSMBFileSharesResponse pHttpStatus_ =
  DescribeSMBFileSharesResponse'
    { sMBFileShareInfoList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing a description for each requested file share.
describeSMBFileSharesResponse_sMBFileShareInfoList :: Lens.Lens' DescribeSMBFileSharesResponse (Prelude.Maybe [SMBFileShareInfo])
describeSMBFileSharesResponse_sMBFileShareInfoList = Lens.lens (\DescribeSMBFileSharesResponse' {sMBFileShareInfoList} -> sMBFileShareInfoList) (\s@DescribeSMBFileSharesResponse' {} a -> s {sMBFileShareInfoList = a} :: DescribeSMBFileSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSMBFileSharesResponse_httpStatus :: Lens.Lens' DescribeSMBFileSharesResponse Prelude.Int
describeSMBFileSharesResponse_httpStatus = Lens.lens (\DescribeSMBFileSharesResponse' {httpStatus} -> httpStatus) (\s@DescribeSMBFileSharesResponse' {} a -> s {httpStatus = a} :: DescribeSMBFileSharesResponse)

instance Prelude.NFData DescribeSMBFileSharesResponse
