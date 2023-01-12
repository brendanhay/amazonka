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
-- Module      : Amazonka.StorageGateway.DescribeNFSFileShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Network File System (NFS) file shares
-- from an S3 File Gateway. This operation is only supported for S3 File
-- Gateways.
module Amazonka.StorageGateway.DescribeNFSFileShares
  ( -- * Creating a Request
    DescribeNFSFileShares (..),
    newDescribeNFSFileShares,

    -- * Request Lenses
    describeNFSFileShares_fileShareARNList,

    -- * Destructuring the Response
    DescribeNFSFileSharesResponse (..),
    newDescribeNFSFileSharesResponse,

    -- * Response Lenses
    describeNFSFileSharesResponse_nFSFileShareInfoList,
    describeNFSFileSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | DescribeNFSFileSharesInput
--
-- /See:/ 'newDescribeNFSFileShares' smart constructor.
data DescribeNFSFileShares = DescribeNFSFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to
    -- be described.
    fileShareARNList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNFSFileShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARNList', 'describeNFSFileShares_fileShareARNList' - An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
newDescribeNFSFileShares ::
  -- | 'fileShareARNList'
  Prelude.NonEmpty Prelude.Text ->
  DescribeNFSFileShares
newDescribeNFSFileShares pFileShareARNList_ =
  DescribeNFSFileShares'
    { fileShareARNList =
        Lens.coerced Lens.# pFileShareARNList_
    }

-- | An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
describeNFSFileShares_fileShareARNList :: Lens.Lens' DescribeNFSFileShares (Prelude.NonEmpty Prelude.Text)
describeNFSFileShares_fileShareARNList = Lens.lens (\DescribeNFSFileShares' {fileShareARNList} -> fileShareARNList) (\s@DescribeNFSFileShares' {} a -> s {fileShareARNList = a} :: DescribeNFSFileShares) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeNFSFileShares where
  type
    AWSResponse DescribeNFSFileShares =
      DescribeNFSFileSharesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNFSFileSharesResponse'
            Prelude.<$> ( x Data..?> "NFSFileShareInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNFSFileShares where
  hashWithSalt _salt DescribeNFSFileShares' {..} =
    _salt `Prelude.hashWithSalt` fileShareARNList

instance Prelude.NFData DescribeNFSFileShares where
  rnf DescribeNFSFileShares' {..} =
    Prelude.rnf fileShareARNList

instance Data.ToHeaders DescribeNFSFileShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeNFSFileShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeNFSFileShares where
  toJSON DescribeNFSFileShares' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FileShareARNList" Data..= fileShareARNList)
          ]
      )

instance Data.ToPath DescribeNFSFileShares where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeNFSFileShares where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeNFSFileSharesOutput
--
-- /See:/ 'newDescribeNFSFileSharesResponse' smart constructor.
data DescribeNFSFileSharesResponse = DescribeNFSFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    nFSFileShareInfoList :: Prelude.Maybe [NFSFileShareInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNFSFileSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nFSFileShareInfoList', 'describeNFSFileSharesResponse_nFSFileShareInfoList' - An array containing a description for each requested file share.
--
-- 'httpStatus', 'describeNFSFileSharesResponse_httpStatus' - The response's http status code.
newDescribeNFSFileSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNFSFileSharesResponse
newDescribeNFSFileSharesResponse pHttpStatus_ =
  DescribeNFSFileSharesResponse'
    { nFSFileShareInfoList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing a description for each requested file share.
describeNFSFileSharesResponse_nFSFileShareInfoList :: Lens.Lens' DescribeNFSFileSharesResponse (Prelude.Maybe [NFSFileShareInfo])
describeNFSFileSharesResponse_nFSFileShareInfoList = Lens.lens (\DescribeNFSFileSharesResponse' {nFSFileShareInfoList} -> nFSFileShareInfoList) (\s@DescribeNFSFileSharesResponse' {} a -> s {nFSFileShareInfoList = a} :: DescribeNFSFileSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNFSFileSharesResponse_httpStatus :: Lens.Lens' DescribeNFSFileSharesResponse Prelude.Int
describeNFSFileSharesResponse_httpStatus = Lens.lens (\DescribeNFSFileSharesResponse' {httpStatus} -> httpStatus) (\s@DescribeNFSFileSharesResponse' {} a -> s {httpStatus = a} :: DescribeNFSFileSharesResponse)

instance Prelude.NFData DescribeNFSFileSharesResponse where
  rnf DescribeNFSFileSharesResponse' {..} =
    Prelude.rnf nFSFileShareInfoList
      `Prelude.seq` Prelude.rnf httpStatus
