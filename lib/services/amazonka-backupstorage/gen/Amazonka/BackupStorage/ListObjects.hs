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
-- Module      : Amazonka.BackupStorage.ListObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all Objects in a given Backup.
module Amazonka.BackupStorage.ListObjects
  ( -- * Creating a Request
    ListObjects (..),
    newListObjects,

    -- * Request Lenses
    listObjects_nextToken,
    listObjects_createdBefore,
    listObjects_startingObjectName,
    listObjects_maxResults,
    listObjects_startingObjectPrefix,
    listObjects_createdAfter,
    listObjects_storageJobId,

    -- * Destructuring the Response
    ListObjectsResponse (..),
    newListObjectsResponse,

    -- * Response Lenses
    listObjectsResponse_nextToken,
    listObjectsResponse_httpStatus,
    listObjectsResponse_objectList,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObjects' smart constructor.
data ListObjects = ListObjects'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Created before filter
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | Optional, specifies the starting Object name to list from. Ignored if
    -- NextToken is not NULL
    startingObjectName :: Prelude.Maybe Prelude.Text,
    -- | Maximum objects count
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional, specifies the starting Object prefix to list from. Ignored if
    -- NextToken is not NULL
    startingObjectPrefix :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Created after filter
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | Storage job id
    storageJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjects_nextToken' - Pagination token
--
-- 'createdBefore', 'listObjects_createdBefore' - (Optional) Created before filter
--
-- 'startingObjectName', 'listObjects_startingObjectName' - Optional, specifies the starting Object name to list from. Ignored if
-- NextToken is not NULL
--
-- 'maxResults', 'listObjects_maxResults' - Maximum objects count
--
-- 'startingObjectPrefix', 'listObjects_startingObjectPrefix' - Optional, specifies the starting Object prefix to list from. Ignored if
-- NextToken is not NULL
--
-- 'createdAfter', 'listObjects_createdAfter' - (Optional) Created after filter
--
-- 'storageJobId', 'listObjects_storageJobId' - Storage job id
newListObjects ::
  -- | 'storageJobId'
  Prelude.Text ->
  ListObjects
newListObjects pStorageJobId_ =
  ListObjects'
    { nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      startingObjectName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startingObjectPrefix = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      storageJobId = pStorageJobId_
    }

-- | Pagination token
listObjects_nextToken :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_nextToken = Lens.lens (\ListObjects' {nextToken} -> nextToken) (\s@ListObjects' {} a -> s {nextToken = a} :: ListObjects)

-- | (Optional) Created before filter
listObjects_createdBefore :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.UTCTime)
listObjects_createdBefore = Lens.lens (\ListObjects' {createdBefore} -> createdBefore) (\s@ListObjects' {} a -> s {createdBefore = a} :: ListObjects) Prelude.. Lens.mapping Data._Time

-- | Optional, specifies the starting Object name to list from. Ignored if
-- NextToken is not NULL
listObjects_startingObjectName :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_startingObjectName = Lens.lens (\ListObjects' {startingObjectName} -> startingObjectName) (\s@ListObjects' {} a -> s {startingObjectName = a} :: ListObjects)

-- | Maximum objects count
listObjects_maxResults :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Natural)
listObjects_maxResults = Lens.lens (\ListObjects' {maxResults} -> maxResults) (\s@ListObjects' {} a -> s {maxResults = a} :: ListObjects)

-- | Optional, specifies the starting Object prefix to list from. Ignored if
-- NextToken is not NULL
listObjects_startingObjectPrefix :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_startingObjectPrefix = Lens.lens (\ListObjects' {startingObjectPrefix} -> startingObjectPrefix) (\s@ListObjects' {} a -> s {startingObjectPrefix = a} :: ListObjects)

-- | (Optional) Created after filter
listObjects_createdAfter :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.UTCTime)
listObjects_createdAfter = Lens.lens (\ListObjects' {createdAfter} -> createdAfter) (\s@ListObjects' {} a -> s {createdAfter = a} :: ListObjects) Prelude.. Lens.mapping Data._Time

-- | Storage job id
listObjects_storageJobId :: Lens.Lens' ListObjects Prelude.Text
listObjects_storageJobId = Lens.lens (\ListObjects' {storageJobId} -> storageJobId) (\s@ListObjects' {} a -> s {storageJobId = a} :: ListObjects)

instance Core.AWSRequest ListObjects where
  type AWSResponse ListObjects = ListObjectsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ObjectList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListObjects where
  hashWithSalt _salt ListObjects' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` startingObjectName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startingObjectPrefix
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` storageJobId

instance Prelude.NFData ListObjects where
  rnf ListObjects' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf startingObjectName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startingObjectPrefix
      `Prelude.seq` Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf storageJobId

instance Data.ToHeaders ListObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListObjects where
  toPath ListObjects' {..} =
    Prelude.mconcat
      [ "/restore-jobs/",
        Data.toBS storageJobId,
        "/objects/list"
      ]

instance Data.ToQuery ListObjects where
  toQuery ListObjects' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "created-before" Data.=: createdBefore,
        "starting-object-name" Data.=: startingObjectName,
        "max-results" Data.=: maxResults,
        "starting-object-prefix"
          Data.=: startingObjectPrefix,
        "created-after" Data.=: createdAfter
      ]

-- | /See:/ 'newListObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Object list
    objectList :: [BackupObject]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectsResponse_nextToken' - Pagination token
--
-- 'httpStatus', 'listObjectsResponse_httpStatus' - The response's http status code.
--
-- 'objectList', 'listObjectsResponse_objectList' - Object list
newListObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectsResponse
newListObjectsResponse pHttpStatus_ =
  ListObjectsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      objectList = Prelude.mempty
    }

-- | Pagination token
listObjectsResponse_nextToken :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Text)
listObjectsResponse_nextToken = Lens.lens (\ListObjectsResponse' {nextToken} -> nextToken) (\s@ListObjectsResponse' {} a -> s {nextToken = a} :: ListObjectsResponse)

-- | The response's http status code.
listObjectsResponse_httpStatus :: Lens.Lens' ListObjectsResponse Prelude.Int
listObjectsResponse_httpStatus = Lens.lens (\ListObjectsResponse' {httpStatus} -> httpStatus) (\s@ListObjectsResponse' {} a -> s {httpStatus = a} :: ListObjectsResponse)

-- | Object list
listObjectsResponse_objectList :: Lens.Lens' ListObjectsResponse [BackupObject]
listObjectsResponse_objectList = Lens.lens (\ListObjectsResponse' {objectList} -> objectList) (\s@ListObjectsResponse' {} a -> s {objectList = a} :: ListObjectsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListObjectsResponse where
  rnf ListObjectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf objectList
