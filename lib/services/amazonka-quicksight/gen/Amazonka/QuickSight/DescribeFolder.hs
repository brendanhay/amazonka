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
-- Module      : Amazonka.QuickSight.DescribeFolder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a folder.
module Amazonka.QuickSight.DescribeFolder
  ( -- * Creating a Request
    DescribeFolder (..),
    newDescribeFolder,

    -- * Request Lenses
    describeFolder_awsAccountId,
    describeFolder_folderId,

    -- * Destructuring the Response
    DescribeFolderResponse (..),
    newDescribeFolderResponse,

    -- * Response Lenses
    describeFolderResponse_folder,
    describeFolderResponse_requestId,
    describeFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFolder' smart constructor.
data DescribeFolder = DescribeFolder'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeFolder_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'describeFolder_folderId' - The ID of the folder.
newDescribeFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  DescribeFolder
newDescribeFolder pAwsAccountId_ pFolderId_ =
  DescribeFolder'
    { awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The ID for the Amazon Web Services account that contains the folder.
describeFolder_awsAccountId :: Lens.Lens' DescribeFolder Prelude.Text
describeFolder_awsAccountId = Lens.lens (\DescribeFolder' {awsAccountId} -> awsAccountId) (\s@DescribeFolder' {} a -> s {awsAccountId = a} :: DescribeFolder)

-- | The ID of the folder.
describeFolder_folderId :: Lens.Lens' DescribeFolder Prelude.Text
describeFolder_folderId = Lens.lens (\DescribeFolder' {folderId} -> folderId) (\s@DescribeFolder' {} a -> s {folderId = a} :: DescribeFolder)

instance Core.AWSRequest DescribeFolder where
  type
    AWSResponse DescribeFolder =
      DescribeFolderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderResponse'
            Prelude.<$> (x Data..?> "Folder")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFolder where
  hashWithSalt _salt DescribeFolder' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DescribeFolder where
  rnf DescribeFolder' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders DescribeFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeFolder where
  toPath DescribeFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId
      ]

instance Data.ToQuery DescribeFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFolderResponse' smart constructor.
data DescribeFolderResponse = DescribeFolderResponse'
  { -- | Information about the folder.
    folder :: Prelude.Maybe Folder,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folder', 'describeFolderResponse_folder' - Information about the folder.
--
-- 'requestId', 'describeFolderResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeFolderResponse_status' - The HTTP status of the request.
newDescribeFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeFolderResponse
newDescribeFolderResponse pStatus_ =
  DescribeFolderResponse'
    { folder = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Information about the folder.
describeFolderResponse_folder :: Lens.Lens' DescribeFolderResponse (Prelude.Maybe Folder)
describeFolderResponse_folder = Lens.lens (\DescribeFolderResponse' {folder} -> folder) (\s@DescribeFolderResponse' {} a -> s {folder = a} :: DescribeFolderResponse)

-- | The Amazon Web Services request ID for this operation.
describeFolderResponse_requestId :: Lens.Lens' DescribeFolderResponse (Prelude.Maybe Prelude.Text)
describeFolderResponse_requestId = Lens.lens (\DescribeFolderResponse' {requestId} -> requestId) (\s@DescribeFolderResponse' {} a -> s {requestId = a} :: DescribeFolderResponse)

-- | The HTTP status of the request.
describeFolderResponse_status :: Lens.Lens' DescribeFolderResponse Prelude.Int
describeFolderResponse_status = Lens.lens (\DescribeFolderResponse' {status} -> status) (\s@DescribeFolderResponse' {} a -> s {status = a} :: DescribeFolderResponse)

instance Prelude.NFData DescribeFolderResponse where
  rnf DescribeFolderResponse' {..} =
    Prelude.rnf folder
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
