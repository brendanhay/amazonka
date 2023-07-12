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
-- Module      : Amazonka.StorageGateway.DescribeSMBFileShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Server Message Block (SMB) file
-- shares from a S3 File Gateway. This operation is only supported for S3
-- File Gateways.
module Amazonka.StorageGateway.DescribeSMBFileShares
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSMBFileSharesResponse'
            Prelude.<$> ( x
                            Data..?> "SMBFileShareInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSMBFileShares where
  hashWithSalt _salt DescribeSMBFileShares' {..} =
    _salt `Prelude.hashWithSalt` fileShareARNList

instance Prelude.NFData DescribeSMBFileShares where
  rnf DescribeSMBFileShares' {..} =
    Prelude.rnf fileShareARNList

instance Data.ToHeaders DescribeSMBFileShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeSMBFileShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSMBFileShares where
  toJSON DescribeSMBFileShares' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FileShareARNList" Data..= fileShareARNList)
          ]
      )

instance Data.ToPath DescribeSMBFileShares where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSMBFileShares where
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

instance Prelude.NFData DescribeSMBFileSharesResponse where
  rnf DescribeSMBFileSharesResponse' {..} =
    Prelude.rnf sMBFileShareInfoList
      `Prelude.seq` Prelude.rnf httpStatus
