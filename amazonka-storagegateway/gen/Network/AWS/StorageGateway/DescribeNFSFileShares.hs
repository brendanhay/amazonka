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
-- Module      : Network.AWS.StorageGateway.DescribeNFSFileShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Network File System (NFS) file shares
-- from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeNFSFileShares
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeNFSFileSharesInput
--
-- /See:/ 'newDescribeNFSFileShares' smart constructor.
data DescribeNFSFileShares = DescribeNFSFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to
    -- be described.
    fileShareARNList :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeNFSFileShares
newDescribeNFSFileShares pFileShareARNList_ =
  DescribeNFSFileShares'
    { fileShareARNList =
        Lens._Coerce Lens.# pFileShareARNList_
    }

-- | An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
describeNFSFileShares_fileShareARNList :: Lens.Lens' DescribeNFSFileShares (Core.NonEmpty Core.Text)
describeNFSFileShares_fileShareARNList = Lens.lens (\DescribeNFSFileShares' {fileShareARNList} -> fileShareARNList) (\s@DescribeNFSFileShares' {} a -> s {fileShareARNList = a} :: DescribeNFSFileShares) Core.. Lens._Coerce

instance Core.AWSRequest DescribeNFSFileShares where
  type
    AWSResponse DescribeNFSFileShares =
      DescribeNFSFileSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNFSFileSharesResponse'
            Core.<$> ( x Core..?> "NFSFileShareInfoList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNFSFileShares

instance Core.NFData DescribeNFSFileShares

instance Core.ToHeaders DescribeNFSFileShares where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeNFSFileShares" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeNFSFileShares where
  toJSON DescribeNFSFileShares' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("FileShareARNList" Core..= fileShareARNList)
          ]
      )

instance Core.ToPath DescribeNFSFileShares where
  toPath = Core.const "/"

instance Core.ToQuery DescribeNFSFileShares where
  toQuery = Core.const Core.mempty

-- | DescribeNFSFileSharesOutput
--
-- /See:/ 'newDescribeNFSFileSharesResponse' smart constructor.
data DescribeNFSFileSharesResponse = DescribeNFSFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    nFSFileShareInfoList :: Core.Maybe [NFSFileShareInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeNFSFileSharesResponse
newDescribeNFSFileSharesResponse pHttpStatus_ =
  DescribeNFSFileSharesResponse'
    { nFSFileShareInfoList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing a description for each requested file share.
describeNFSFileSharesResponse_nFSFileShareInfoList :: Lens.Lens' DescribeNFSFileSharesResponse (Core.Maybe [NFSFileShareInfo])
describeNFSFileSharesResponse_nFSFileShareInfoList = Lens.lens (\DescribeNFSFileSharesResponse' {nFSFileShareInfoList} -> nFSFileShareInfoList) (\s@DescribeNFSFileSharesResponse' {} a -> s {nFSFileShareInfoList = a} :: DescribeNFSFileSharesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNFSFileSharesResponse_httpStatus :: Lens.Lens' DescribeNFSFileSharesResponse Core.Int
describeNFSFileSharesResponse_httpStatus = Lens.lens (\DescribeNFSFileSharesResponse' {httpStatus} -> httpStatus) (\s@DescribeNFSFileSharesResponse' {} a -> s {httpStatus = a} :: DescribeNFSFileSharesResponse)

instance Core.NFData DescribeNFSFileSharesResponse
