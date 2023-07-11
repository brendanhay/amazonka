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
-- Module      : Amazonka.OpenSearchServerless.BatchGetCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes for one or more collections, including the collection
-- endpoint and the OpenSearch Dashboards endpoint. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-manage.html Creating and managing Amazon OpenSearch Serverless collections>.
module Amazonka.OpenSearchServerless.BatchGetCollection
  ( -- * Creating a Request
    BatchGetCollection (..),
    newBatchGetCollection,

    -- * Request Lenses
    batchGetCollection_ids,
    batchGetCollection_names,

    -- * Destructuring the Response
    BatchGetCollectionResponse (..),
    newBatchGetCollectionResponse,

    -- * Response Lenses
    batchGetCollectionResponse_collectionDetails,
    batchGetCollectionResponse_collectionErrorDetails,
    batchGetCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetCollection' smart constructor.
data BatchGetCollection = BatchGetCollection'
  { -- | A list of collection IDs. You can\'t provide names and IDs in the same
    -- request. The ID is part of the collection endpoint. You can also
    -- retrieve it using the
    -- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
    -- API.
    ids :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of collection names. You can\'t provide names and IDs in the same
    -- request.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ids', 'batchGetCollection_ids' - A list of collection IDs. You can\'t provide names and IDs in the same
-- request. The ID is part of the collection endpoint. You can also
-- retrieve it using the
-- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
-- API.
--
-- 'names', 'batchGetCollection_names' - A list of collection names. You can\'t provide names and IDs in the same
-- request.
newBatchGetCollection ::
  BatchGetCollection
newBatchGetCollection =
  BatchGetCollection'
    { ids = Prelude.Nothing,
      names = Prelude.Nothing
    }

-- | A list of collection IDs. You can\'t provide names and IDs in the same
-- request. The ID is part of the collection endpoint. You can also
-- retrieve it using the
-- <https://docs.aws.amazon.com/opensearch-service/latest/ServerlessAPIReference/API_ListCollections.html ListCollections>
-- API.
batchGetCollection_ids :: Lens.Lens' BatchGetCollection (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetCollection_ids = Lens.lens (\BatchGetCollection' {ids} -> ids) (\s@BatchGetCollection' {} a -> s {ids = a} :: BatchGetCollection) Prelude.. Lens.mapping Lens.coerced

-- | A list of collection names. You can\'t provide names and IDs in the same
-- request.
batchGetCollection_names :: Lens.Lens' BatchGetCollection (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetCollection_names = Lens.lens (\BatchGetCollection' {names} -> names) (\s@BatchGetCollection' {} a -> s {names = a} :: BatchGetCollection) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest BatchGetCollection where
  type
    AWSResponse BatchGetCollection =
      BatchGetCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCollectionResponse'
            Prelude.<$> ( x
                            Data..?> "collectionDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "collectionErrorDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCollection where
  hashWithSalt _salt BatchGetCollection' {..} =
    _salt
      `Prelude.hashWithSalt` ids
      `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetCollection where
  rnf BatchGetCollection' {..} =
    Prelude.rnf ids `Prelude.seq` Prelude.rnf names

instance Data.ToHeaders BatchGetCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.BatchGetCollection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetCollection where
  toJSON BatchGetCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ids" Data..=) Prelude.<$> ids,
            ("names" Data..=) Prelude.<$> names
          ]
      )

instance Data.ToPath BatchGetCollection where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetCollectionResponse' smart constructor.
data BatchGetCollectionResponse = BatchGetCollectionResponse'
  { -- | Details about each collection.
    collectionDetails :: Prelude.Maybe [CollectionDetail],
    -- | Error information for the request.
    collectionErrorDetails :: Prelude.Maybe [CollectionErrorDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionDetails', 'batchGetCollectionResponse_collectionDetails' - Details about each collection.
--
-- 'collectionErrorDetails', 'batchGetCollectionResponse_collectionErrorDetails' - Error information for the request.
--
-- 'httpStatus', 'batchGetCollectionResponse_httpStatus' - The response's http status code.
newBatchGetCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetCollectionResponse
newBatchGetCollectionResponse pHttpStatus_ =
  BatchGetCollectionResponse'
    { collectionDetails =
        Prelude.Nothing,
      collectionErrorDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about each collection.
batchGetCollectionResponse_collectionDetails :: Lens.Lens' BatchGetCollectionResponse (Prelude.Maybe [CollectionDetail])
batchGetCollectionResponse_collectionDetails = Lens.lens (\BatchGetCollectionResponse' {collectionDetails} -> collectionDetails) (\s@BatchGetCollectionResponse' {} a -> s {collectionDetails = a} :: BatchGetCollectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Error information for the request.
batchGetCollectionResponse_collectionErrorDetails :: Lens.Lens' BatchGetCollectionResponse (Prelude.Maybe [CollectionErrorDetail])
batchGetCollectionResponse_collectionErrorDetails = Lens.lens (\BatchGetCollectionResponse' {collectionErrorDetails} -> collectionErrorDetails) (\s@BatchGetCollectionResponse' {} a -> s {collectionErrorDetails = a} :: BatchGetCollectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetCollectionResponse_httpStatus :: Lens.Lens' BatchGetCollectionResponse Prelude.Int
batchGetCollectionResponse_httpStatus = Lens.lens (\BatchGetCollectionResponse' {httpStatus} -> httpStatus) (\s@BatchGetCollectionResponse' {} a -> s {httpStatus = a} :: BatchGetCollectionResponse)

instance Prelude.NFData BatchGetCollectionResponse where
  rnf BatchGetCollectionResponse' {..} =
    Prelude.rnf collectionDetails
      `Prelude.seq` Prelude.rnf collectionErrorDetails
      `Prelude.seq` Prelude.rnf httpStatus
