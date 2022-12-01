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
-- Module      : Amazonka.StorageGateway.DescribeFileSystemAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the file system association information. This operation is only
-- supported for FSx File Gateways.
module Amazonka.StorageGateway.DescribeFileSystemAssociations
  ( -- * Creating a Request
    DescribeFileSystemAssociations (..),
    newDescribeFileSystemAssociations,

    -- * Request Lenses
    describeFileSystemAssociations_fileSystemAssociationARNList,

    -- * Destructuring the Response
    DescribeFileSystemAssociationsResponse (..),
    newDescribeFileSystemAssociationsResponse,

    -- * Response Lenses
    describeFileSystemAssociationsResponse_fileSystemAssociationInfoList,
    describeFileSystemAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDescribeFileSystemAssociations' smart constructor.
data DescribeFileSystemAssociations = DescribeFileSystemAssociations'
  { -- | An array containing the Amazon Resource Name (ARN) of each file system
    -- association to be described.
    fileSystemAssociationARNList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystemAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationARNList', 'describeFileSystemAssociations_fileSystemAssociationARNList' - An array containing the Amazon Resource Name (ARN) of each file system
-- association to be described.
newDescribeFileSystemAssociations ::
  -- | 'fileSystemAssociationARNList'
  Prelude.NonEmpty Prelude.Text ->
  DescribeFileSystemAssociations
newDescribeFileSystemAssociations
  pFileSystemAssociationARNList_ =
    DescribeFileSystemAssociations'
      { fileSystemAssociationARNList =
          Lens.coerced
            Lens.# pFileSystemAssociationARNList_
      }

-- | An array containing the Amazon Resource Name (ARN) of each file system
-- association to be described.
describeFileSystemAssociations_fileSystemAssociationARNList :: Lens.Lens' DescribeFileSystemAssociations (Prelude.NonEmpty Prelude.Text)
describeFileSystemAssociations_fileSystemAssociationARNList = Lens.lens (\DescribeFileSystemAssociations' {fileSystemAssociationARNList} -> fileSystemAssociationARNList) (\s@DescribeFileSystemAssociations' {} a -> s {fileSystemAssociationARNList = a} :: DescribeFileSystemAssociations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DescribeFileSystemAssociations
  where
  type
    AWSResponse DescribeFileSystemAssociations =
      DescribeFileSystemAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFileSystemAssociationsResponse'
            Prelude.<$> ( x Core..?> "FileSystemAssociationInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFileSystemAssociations
  where
  hashWithSalt
    _salt
    DescribeFileSystemAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` fileSystemAssociationARNList

instance
  Prelude.NFData
    DescribeFileSystemAssociations
  where
  rnf DescribeFileSystemAssociations' {..} =
    Prelude.rnf fileSystemAssociationARNList

instance
  Core.ToHeaders
    DescribeFileSystemAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeFileSystemAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFileSystemAssociations where
  toJSON DescribeFileSystemAssociations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FileSystemAssociationARNList"
                  Core..= fileSystemAssociationARNList
              )
          ]
      )

instance Core.ToPath DescribeFileSystemAssociations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFileSystemAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFileSystemAssociationsResponse' smart constructor.
data DescribeFileSystemAssociationsResponse = DescribeFileSystemAssociationsResponse'
  { -- | An array containing the @FileSystemAssociationInfo@ data type of each
    -- file system association to be described.
    fileSystemAssociationInfoList :: Prelude.Maybe [FileSystemAssociationInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileSystemAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationInfoList', 'describeFileSystemAssociationsResponse_fileSystemAssociationInfoList' - An array containing the @FileSystemAssociationInfo@ data type of each
-- file system association to be described.
--
-- 'httpStatus', 'describeFileSystemAssociationsResponse_httpStatus' - The response's http status code.
newDescribeFileSystemAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFileSystemAssociationsResponse
newDescribeFileSystemAssociationsResponse
  pHttpStatus_ =
    DescribeFileSystemAssociationsResponse'
      { fileSystemAssociationInfoList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array containing the @FileSystemAssociationInfo@ data type of each
-- file system association to be described.
describeFileSystemAssociationsResponse_fileSystemAssociationInfoList :: Lens.Lens' DescribeFileSystemAssociationsResponse (Prelude.Maybe [FileSystemAssociationInfo])
describeFileSystemAssociationsResponse_fileSystemAssociationInfoList = Lens.lens (\DescribeFileSystemAssociationsResponse' {fileSystemAssociationInfoList} -> fileSystemAssociationInfoList) (\s@DescribeFileSystemAssociationsResponse' {} a -> s {fileSystemAssociationInfoList = a} :: DescribeFileSystemAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFileSystemAssociationsResponse_httpStatus :: Lens.Lens' DescribeFileSystemAssociationsResponse Prelude.Int
describeFileSystemAssociationsResponse_httpStatus = Lens.lens (\DescribeFileSystemAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeFileSystemAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeFileSystemAssociationsResponse)

instance
  Prelude.NFData
    DescribeFileSystemAssociationsResponse
  where
  rnf DescribeFileSystemAssociationsResponse' {..} =
    Prelude.rnf fileSystemAssociationInfoList
      `Prelude.seq` Prelude.rnf httpStatus
