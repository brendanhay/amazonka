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
-- Module      : Amazonka.DirectoryService.DescribeUpdateDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the updates of a directory for a particular update type.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.DescribeUpdateDirectory
  ( -- * Creating a Request
    DescribeUpdateDirectory (..),
    newDescribeUpdateDirectory,

    -- * Request Lenses
    describeUpdateDirectory_nextToken,
    describeUpdateDirectory_regionName,
    describeUpdateDirectory_directoryId,
    describeUpdateDirectory_updateType,

    -- * Destructuring the Response
    DescribeUpdateDirectoryResponse (..),
    newDescribeUpdateDirectoryResponse,

    -- * Response Lenses
    describeUpdateDirectoryResponse_nextToken,
    describeUpdateDirectoryResponse_updateActivities,
    describeUpdateDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUpdateDirectory' smart constructor.
data DescribeUpdateDirectory = DescribeUpdateDirectory'
  { -- | The @DescribeUpdateDirectoryResult@. NextToken value from a previous
    -- call to DescribeUpdateDirectory. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The type of updates you want to describe for the directory.
    updateType :: UpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUpdateDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUpdateDirectory_nextToken' - The @DescribeUpdateDirectoryResult@. NextToken value from a previous
-- call to DescribeUpdateDirectory. Pass null if this is the first call.
--
-- 'regionName', 'describeUpdateDirectory_regionName' - The name of the Region.
--
-- 'directoryId', 'describeUpdateDirectory_directoryId' - The unique identifier of the directory.
--
-- 'updateType', 'describeUpdateDirectory_updateType' - The type of updates you want to describe for the directory.
newDescribeUpdateDirectory ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'updateType'
  UpdateType ->
  DescribeUpdateDirectory
newDescribeUpdateDirectory pDirectoryId_ pUpdateType_ =
  DescribeUpdateDirectory'
    { nextToken =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      directoryId = pDirectoryId_,
      updateType = pUpdateType_
    }

-- | The @DescribeUpdateDirectoryResult@. NextToken value from a previous
-- call to DescribeUpdateDirectory. Pass null if this is the first call.
describeUpdateDirectory_nextToken :: Lens.Lens' DescribeUpdateDirectory (Prelude.Maybe Prelude.Text)
describeUpdateDirectory_nextToken = Lens.lens (\DescribeUpdateDirectory' {nextToken} -> nextToken) (\s@DescribeUpdateDirectory' {} a -> s {nextToken = a} :: DescribeUpdateDirectory)

-- | The name of the Region.
describeUpdateDirectory_regionName :: Lens.Lens' DescribeUpdateDirectory (Prelude.Maybe Prelude.Text)
describeUpdateDirectory_regionName = Lens.lens (\DescribeUpdateDirectory' {regionName} -> regionName) (\s@DescribeUpdateDirectory' {} a -> s {regionName = a} :: DescribeUpdateDirectory)

-- | The unique identifier of the directory.
describeUpdateDirectory_directoryId :: Lens.Lens' DescribeUpdateDirectory Prelude.Text
describeUpdateDirectory_directoryId = Lens.lens (\DescribeUpdateDirectory' {directoryId} -> directoryId) (\s@DescribeUpdateDirectory' {} a -> s {directoryId = a} :: DescribeUpdateDirectory)

-- | The type of updates you want to describe for the directory.
describeUpdateDirectory_updateType :: Lens.Lens' DescribeUpdateDirectory UpdateType
describeUpdateDirectory_updateType = Lens.lens (\DescribeUpdateDirectory' {updateType} -> updateType) (\s@DescribeUpdateDirectory' {} a -> s {updateType = a} :: DescribeUpdateDirectory)

instance Core.AWSPager DescribeUpdateDirectory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUpdateDirectoryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUpdateDirectoryResponse_updateActivities
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeUpdateDirectory_nextToken
              Lens..~ rs
              Lens.^? describeUpdateDirectoryResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeUpdateDirectory where
  type
    AWSResponse DescribeUpdateDirectory =
      DescribeUpdateDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUpdateDirectoryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "UpdateActivities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUpdateDirectory where
  hashWithSalt _salt DescribeUpdateDirectory' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData DescribeUpdateDirectory where
  rnf DescribeUpdateDirectory' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf regionName `Prelude.seq`
        Prelude.rnf directoryId `Prelude.seq`
          Prelude.rnf updateType

instance Data.ToHeaders DescribeUpdateDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeUpdateDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUpdateDirectory where
  toJSON DescribeUpdateDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RegionName" Data..=) Prelude.<$> regionName,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("UpdateType" Data..= updateType)
          ]
      )

instance Data.ToPath DescribeUpdateDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUpdateDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUpdateDirectoryResponse' smart constructor.
data DescribeUpdateDirectoryResponse = DescribeUpdateDirectoryResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of update activities on a directory for the requested update
    -- type.
    updateActivities :: Prelude.Maybe [UpdateInfoEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUpdateDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUpdateDirectoryResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter.
--
-- 'updateActivities', 'describeUpdateDirectoryResponse_updateActivities' - The list of update activities on a directory for the requested update
-- type.
--
-- 'httpStatus', 'describeUpdateDirectoryResponse_httpStatus' - The response's http status code.
newDescribeUpdateDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUpdateDirectoryResponse
newDescribeUpdateDirectoryResponse pHttpStatus_ =
  DescribeUpdateDirectoryResponse'
    { nextToken =
        Prelude.Nothing,
      updateActivities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter.
describeUpdateDirectoryResponse_nextToken :: Lens.Lens' DescribeUpdateDirectoryResponse (Prelude.Maybe Prelude.Text)
describeUpdateDirectoryResponse_nextToken = Lens.lens (\DescribeUpdateDirectoryResponse' {nextToken} -> nextToken) (\s@DescribeUpdateDirectoryResponse' {} a -> s {nextToken = a} :: DescribeUpdateDirectoryResponse)

-- | The list of update activities on a directory for the requested update
-- type.
describeUpdateDirectoryResponse_updateActivities :: Lens.Lens' DescribeUpdateDirectoryResponse (Prelude.Maybe [UpdateInfoEntry])
describeUpdateDirectoryResponse_updateActivities = Lens.lens (\DescribeUpdateDirectoryResponse' {updateActivities} -> updateActivities) (\s@DescribeUpdateDirectoryResponse' {} a -> s {updateActivities = a} :: DescribeUpdateDirectoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUpdateDirectoryResponse_httpStatus :: Lens.Lens' DescribeUpdateDirectoryResponse Prelude.Int
describeUpdateDirectoryResponse_httpStatus = Lens.lens (\DescribeUpdateDirectoryResponse' {httpStatus} -> httpStatus) (\s@DescribeUpdateDirectoryResponse' {} a -> s {httpStatus = a} :: DescribeUpdateDirectoryResponse)

instance
  Prelude.NFData
    DescribeUpdateDirectoryResponse
  where
  rnf DescribeUpdateDirectoryResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf updateActivities `Prelude.seq`
        Prelude.rnf httpStatus
