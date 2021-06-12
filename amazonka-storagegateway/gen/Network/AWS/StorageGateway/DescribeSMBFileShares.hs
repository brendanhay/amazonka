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
-- shares from a file gateway. This operation is only supported for file
-- gateways.
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeSMBFileSharesInput
--
-- /See:/ 'newDescribeSMBFileShares' smart constructor.
data DescribeSMBFileShares = DescribeSMBFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to
    -- be described.
    fileShareARNList :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DescribeSMBFileShares
newDescribeSMBFileShares pFileShareARNList_ =
  DescribeSMBFileShares'
    { fileShareARNList =
        Lens._Coerce Lens.# pFileShareARNList_
    }

-- | An array containing the Amazon Resource Name (ARN) of each file share to
-- be described.
describeSMBFileShares_fileShareARNList :: Lens.Lens' DescribeSMBFileShares (Core.NonEmpty Core.Text)
describeSMBFileShares_fileShareARNList = Lens.lens (\DescribeSMBFileShares' {fileShareARNList} -> fileShareARNList) (\s@DescribeSMBFileShares' {} a -> s {fileShareARNList = a} :: DescribeSMBFileShares) Core.. Lens._Coerce

instance Core.AWSRequest DescribeSMBFileShares where
  type
    AWSResponse DescribeSMBFileShares =
      DescribeSMBFileSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSMBFileSharesResponse'
            Core.<$> ( x Core..?> "SMBFileShareInfoList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSMBFileShares

instance Core.NFData DescribeSMBFileShares

instance Core.ToHeaders DescribeSMBFileShares where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeSMBFileShares" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSMBFileShares where
  toJSON DescribeSMBFileShares' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("FileShareARNList" Core..= fileShareARNList)
          ]
      )

instance Core.ToPath DescribeSMBFileShares where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSMBFileShares where
  toQuery = Core.const Core.mempty

-- | DescribeSMBFileSharesOutput
--
-- /See:/ 'newDescribeSMBFileSharesResponse' smart constructor.
data DescribeSMBFileSharesResponse = DescribeSMBFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    sMBFileShareInfoList :: Core.Maybe [SMBFileShareInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeSMBFileSharesResponse
newDescribeSMBFileSharesResponse pHttpStatus_ =
  DescribeSMBFileSharesResponse'
    { sMBFileShareInfoList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing a description for each requested file share.
describeSMBFileSharesResponse_sMBFileShareInfoList :: Lens.Lens' DescribeSMBFileSharesResponse (Core.Maybe [SMBFileShareInfo])
describeSMBFileSharesResponse_sMBFileShareInfoList = Lens.lens (\DescribeSMBFileSharesResponse' {sMBFileShareInfoList} -> sMBFileShareInfoList) (\s@DescribeSMBFileSharesResponse' {} a -> s {sMBFileShareInfoList = a} :: DescribeSMBFileSharesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSMBFileSharesResponse_httpStatus :: Lens.Lens' DescribeSMBFileSharesResponse Core.Int
describeSMBFileSharesResponse_httpStatus = Lens.lens (\DescribeSMBFileSharesResponse' {httpStatus} -> httpStatus) (\s@DescribeSMBFileSharesResponse' {} a -> s {httpStatus = a} :: DescribeSMBFileSharesResponse)

instance Core.NFData DescribeSMBFileSharesResponse
