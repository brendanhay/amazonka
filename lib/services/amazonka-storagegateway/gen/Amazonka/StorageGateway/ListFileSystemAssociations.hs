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
-- Module      : Amazonka.StorageGateway.ListFileSystemAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of @FileSystemAssociationSummary@ objects. Each object
-- contains a summary of a file system association. This operation is only
-- supported for FSx File Gateways.
--
-- This operation returns paginated results.
module Amazonka.StorageGateway.ListFileSystemAssociations
  ( -- * Creating a Request
    ListFileSystemAssociations (..),
    newListFileSystemAssociations,

    -- * Request Lenses
    listFileSystemAssociations_gatewayARN,
    listFileSystemAssociations_limit,
    listFileSystemAssociations_marker,

    -- * Destructuring the Response
    ListFileSystemAssociationsResponse (..),
    newListFileSystemAssociationsResponse,

    -- * Response Lenses
    listFileSystemAssociationsResponse_fileSystemAssociationSummaryList,
    listFileSystemAssociationsResponse_marker,
    listFileSystemAssociationsResponse_nextMarker,
    listFileSystemAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newListFileSystemAssociations' smart constructor.
data ListFileSystemAssociations = ListFileSystemAssociations'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of file system associations to return in the
    -- response. If present, @Limit@ must be an integer with a value greater
    -- than zero. Optional.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Opaque pagination token returned from a previous
    -- @ListFileSystemAssociations@ operation. If present, @Marker@ specifies
    -- where to continue the list from after a previous call to
    -- @ListFileSystemAssociations@. Optional.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFileSystemAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'listFileSystemAssociations_gatewayARN' - Undocumented member.
--
-- 'limit', 'listFileSystemAssociations_limit' - The maximum number of file system associations to return in the
-- response. If present, @Limit@ must be an integer with a value greater
-- than zero. Optional.
--
-- 'marker', 'listFileSystemAssociations_marker' - Opaque pagination token returned from a previous
-- @ListFileSystemAssociations@ operation. If present, @Marker@ specifies
-- where to continue the list from after a previous call to
-- @ListFileSystemAssociations@. Optional.
newListFileSystemAssociations ::
  ListFileSystemAssociations
newListFileSystemAssociations =
  ListFileSystemAssociations'
    { gatewayARN =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Undocumented member.
listFileSystemAssociations_gatewayARN :: Lens.Lens' ListFileSystemAssociations (Prelude.Maybe Prelude.Text)
listFileSystemAssociations_gatewayARN = Lens.lens (\ListFileSystemAssociations' {gatewayARN} -> gatewayARN) (\s@ListFileSystemAssociations' {} a -> s {gatewayARN = a} :: ListFileSystemAssociations)

-- | The maximum number of file system associations to return in the
-- response. If present, @Limit@ must be an integer with a value greater
-- than zero. Optional.
listFileSystemAssociations_limit :: Lens.Lens' ListFileSystemAssociations (Prelude.Maybe Prelude.Natural)
listFileSystemAssociations_limit = Lens.lens (\ListFileSystemAssociations' {limit} -> limit) (\s@ListFileSystemAssociations' {} a -> s {limit = a} :: ListFileSystemAssociations)

-- | Opaque pagination token returned from a previous
-- @ListFileSystemAssociations@ operation. If present, @Marker@ specifies
-- where to continue the list from after a previous call to
-- @ListFileSystemAssociations@. Optional.
listFileSystemAssociations_marker :: Lens.Lens' ListFileSystemAssociations (Prelude.Maybe Prelude.Text)
listFileSystemAssociations_marker = Lens.lens (\ListFileSystemAssociations' {marker} -> marker) (\s@ListFileSystemAssociations' {} a -> s {marker = a} :: ListFileSystemAssociations)

instance Core.AWSPager ListFileSystemAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFileSystemAssociationsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFileSystemAssociationsResponse_fileSystemAssociationSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listFileSystemAssociations_marker
              Lens..~ rs
              Lens.^? listFileSystemAssociationsResponse_nextMarker
              Prelude.. Lens._Just

instance Core.AWSRequest ListFileSystemAssociations where
  type
    AWSResponse ListFileSystemAssociations =
      ListFileSystemAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFileSystemAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "FileSystemAssociationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFileSystemAssociations where
  hashWithSalt _salt ListFileSystemAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker

instance Prelude.NFData ListFileSystemAssociations where
  rnf ListFileSystemAssociations' {..} =
    Prelude.rnf gatewayARN `Prelude.seq`
      Prelude.rnf limit `Prelude.seq`
        Prelude.rnf marker

instance Data.ToHeaders ListFileSystemAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.ListFileSystemAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFileSystemAssociations where
  toJSON ListFileSystemAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GatewayARN" Data..=) Prelude.<$> gatewayARN,
            ("Limit" Data..=) Prelude.<$> limit,
            ("Marker" Data..=) Prelude.<$> marker
          ]
      )

instance Data.ToPath ListFileSystemAssociations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFileSystemAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFileSystemAssociationsResponse' smart constructor.
data ListFileSystemAssociationsResponse = ListFileSystemAssociationsResponse'
  { -- | An array of information about the Amazon FSx gateway\'s file system
    -- associations.
    fileSystemAssociationSummaryList :: Prelude.Maybe [FileSystemAssociationSummary],
    -- | If the request includes @Marker@, the response returns that value in
    -- this field.
    marker :: Prelude.Maybe Prelude.Text,
    -- | If a value is present, there are more file system associations to
    -- return. In a subsequent request, use @NextMarker@ as the value for
    -- @Marker@ to retrieve the next set of file system associations.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFileSystemAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationSummaryList', 'listFileSystemAssociationsResponse_fileSystemAssociationSummaryList' - An array of information about the Amazon FSx gateway\'s file system
-- associations.
--
-- 'marker', 'listFileSystemAssociationsResponse_marker' - If the request includes @Marker@, the response returns that value in
-- this field.
--
-- 'nextMarker', 'listFileSystemAssociationsResponse_nextMarker' - If a value is present, there are more file system associations to
-- return. In a subsequent request, use @NextMarker@ as the value for
-- @Marker@ to retrieve the next set of file system associations.
--
-- 'httpStatus', 'listFileSystemAssociationsResponse_httpStatus' - The response's http status code.
newListFileSystemAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFileSystemAssociationsResponse
newListFileSystemAssociationsResponse pHttpStatus_ =
  ListFileSystemAssociationsResponse'
    { fileSystemAssociationSummaryList =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of information about the Amazon FSx gateway\'s file system
-- associations.
listFileSystemAssociationsResponse_fileSystemAssociationSummaryList :: Lens.Lens' ListFileSystemAssociationsResponse (Prelude.Maybe [FileSystemAssociationSummary])
listFileSystemAssociationsResponse_fileSystemAssociationSummaryList = Lens.lens (\ListFileSystemAssociationsResponse' {fileSystemAssociationSummaryList} -> fileSystemAssociationSummaryList) (\s@ListFileSystemAssociationsResponse' {} a -> s {fileSystemAssociationSummaryList = a} :: ListFileSystemAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the request includes @Marker@, the response returns that value in
-- this field.
listFileSystemAssociationsResponse_marker :: Lens.Lens' ListFileSystemAssociationsResponse (Prelude.Maybe Prelude.Text)
listFileSystemAssociationsResponse_marker = Lens.lens (\ListFileSystemAssociationsResponse' {marker} -> marker) (\s@ListFileSystemAssociationsResponse' {} a -> s {marker = a} :: ListFileSystemAssociationsResponse)

-- | If a value is present, there are more file system associations to
-- return. In a subsequent request, use @NextMarker@ as the value for
-- @Marker@ to retrieve the next set of file system associations.
listFileSystemAssociationsResponse_nextMarker :: Lens.Lens' ListFileSystemAssociationsResponse (Prelude.Maybe Prelude.Text)
listFileSystemAssociationsResponse_nextMarker = Lens.lens (\ListFileSystemAssociationsResponse' {nextMarker} -> nextMarker) (\s@ListFileSystemAssociationsResponse' {} a -> s {nextMarker = a} :: ListFileSystemAssociationsResponse)

-- | The response's http status code.
listFileSystemAssociationsResponse_httpStatus :: Lens.Lens' ListFileSystemAssociationsResponse Prelude.Int
listFileSystemAssociationsResponse_httpStatus = Lens.lens (\ListFileSystemAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListFileSystemAssociationsResponse' {} a -> s {httpStatus = a} :: ListFileSystemAssociationsResponse)

instance
  Prelude.NFData
    ListFileSystemAssociationsResponse
  where
  rnf ListFileSystemAssociationsResponse' {..} =
    Prelude.rnf fileSystemAssociationSummaryList `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf nextMarker `Prelude.seq`
          Prelude.rnf httpStatus
