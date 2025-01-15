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
-- Module      : Amazonka.Athena.ListTableMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metadata for the tables in the specified data catalog
-- database.
--
-- This operation returns paginated results.
module Amazonka.Athena.ListTableMetadata
  ( -- * Creating a Request
    ListTableMetadata (..),
    newListTableMetadata,

    -- * Request Lenses
    listTableMetadata_expression,
    listTableMetadata_maxResults,
    listTableMetadata_nextToken,
    listTableMetadata_catalogName,
    listTableMetadata_databaseName,

    -- * Destructuring the Response
    ListTableMetadataResponse (..),
    newListTableMetadataResponse,

    -- * Response Lenses
    listTableMetadataResponse_nextToken,
    listTableMetadataResponse_tableMetadataList,
    listTableMetadataResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTableMetadata' smart constructor.
data ListTableMetadata = ListTableMetadata'
  { -- | A regex filter that pattern-matches table names. If no expression is
    -- supplied, metadata for all tables are listed.
    expression :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the NextToken from the response object of the previous
    -- page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the data catalog for which table metadata should be
    -- returned.
    catalogName :: Prelude.Text,
    -- | The name of the database for which table metadata should be returned.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'listTableMetadata_expression' - A regex filter that pattern-matches table names. If no expression is
-- supplied, metadata for all tables are listed.
--
-- 'maxResults', 'listTableMetadata_maxResults' - Specifies the maximum number of results to return.
--
-- 'nextToken', 'listTableMetadata_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the NextToken from the response object of the previous
-- page call.
--
-- 'catalogName', 'listTableMetadata_catalogName' - The name of the data catalog for which table metadata should be
-- returned.
--
-- 'databaseName', 'listTableMetadata_databaseName' - The name of the database for which table metadata should be returned.
newListTableMetadata ::
  -- | 'catalogName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  ListTableMetadata
newListTableMetadata pCatalogName_ pDatabaseName_ =
  ListTableMetadata'
    { expression = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      catalogName = pCatalogName_,
      databaseName = pDatabaseName_
    }

-- | A regex filter that pattern-matches table names. If no expression is
-- supplied, metadata for all tables are listed.
listTableMetadata_expression :: Lens.Lens' ListTableMetadata (Prelude.Maybe Prelude.Text)
listTableMetadata_expression = Lens.lens (\ListTableMetadata' {expression} -> expression) (\s@ListTableMetadata' {} a -> s {expression = a} :: ListTableMetadata)

-- | Specifies the maximum number of results to return.
listTableMetadata_maxResults :: Lens.Lens' ListTableMetadata (Prelude.Maybe Prelude.Natural)
listTableMetadata_maxResults = Lens.lens (\ListTableMetadata' {maxResults} -> maxResults) (\s@ListTableMetadata' {} a -> s {maxResults = a} :: ListTableMetadata)

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the NextToken from the response object of the previous
-- page call.
listTableMetadata_nextToken :: Lens.Lens' ListTableMetadata (Prelude.Maybe Prelude.Text)
listTableMetadata_nextToken = Lens.lens (\ListTableMetadata' {nextToken} -> nextToken) (\s@ListTableMetadata' {} a -> s {nextToken = a} :: ListTableMetadata)

-- | The name of the data catalog for which table metadata should be
-- returned.
listTableMetadata_catalogName :: Lens.Lens' ListTableMetadata Prelude.Text
listTableMetadata_catalogName = Lens.lens (\ListTableMetadata' {catalogName} -> catalogName) (\s@ListTableMetadata' {} a -> s {catalogName = a} :: ListTableMetadata)

-- | The name of the database for which table metadata should be returned.
listTableMetadata_databaseName :: Lens.Lens' ListTableMetadata Prelude.Text
listTableMetadata_databaseName = Lens.lens (\ListTableMetadata' {databaseName} -> databaseName) (\s@ListTableMetadata' {} a -> s {databaseName = a} :: ListTableMetadata)

instance Core.AWSPager ListTableMetadata where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTableMetadataResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTableMetadataResponse_tableMetadataList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listTableMetadata_nextToken
              Lens..~ rs
              Lens.^? listTableMetadataResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListTableMetadata where
  type
    AWSResponse ListTableMetadata =
      ListTableMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableMetadataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "TableMetadataList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTableMetadata where
  hashWithSalt _salt ListTableMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` catalogName
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData ListTableMetadata where
  rnf ListTableMetadata' {..} =
    Prelude.rnf expression `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf catalogName `Prelude.seq`
            Prelude.rnf databaseName

instance Data.ToHeaders ListTableMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.ListTableMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTableMetadata where
  toJSON ListTableMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Expression" Data..=) Prelude.<$> expression,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("CatalogName" Data..= catalogName),
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )

instance Data.ToPath ListTableMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTableMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTableMetadataResponse' smart constructor.
data ListTableMetadataResponse = ListTableMetadataResponse'
  { -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the NextToken from the response object of the previous
    -- page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of table metadata.
    tableMetadataList :: Prelude.Maybe [TableMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableMetadataResponse_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the NextToken from the response object of the previous
-- page call.
--
-- 'tableMetadataList', 'listTableMetadataResponse_tableMetadataList' - A list of table metadata.
--
-- 'httpStatus', 'listTableMetadataResponse_httpStatus' - The response's http status code.
newListTableMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTableMetadataResponse
newListTableMetadataResponse pHttpStatus_ =
  ListTableMetadataResponse'
    { nextToken =
        Prelude.Nothing,
      tableMetadataList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the NextToken from the response object of the previous
-- page call.
listTableMetadataResponse_nextToken :: Lens.Lens' ListTableMetadataResponse (Prelude.Maybe Prelude.Text)
listTableMetadataResponse_nextToken = Lens.lens (\ListTableMetadataResponse' {nextToken} -> nextToken) (\s@ListTableMetadataResponse' {} a -> s {nextToken = a} :: ListTableMetadataResponse)

-- | A list of table metadata.
listTableMetadataResponse_tableMetadataList :: Lens.Lens' ListTableMetadataResponse (Prelude.Maybe [TableMetadata])
listTableMetadataResponse_tableMetadataList = Lens.lens (\ListTableMetadataResponse' {tableMetadataList} -> tableMetadataList) (\s@ListTableMetadataResponse' {} a -> s {tableMetadataList = a} :: ListTableMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTableMetadataResponse_httpStatus :: Lens.Lens' ListTableMetadataResponse Prelude.Int
listTableMetadataResponse_httpStatus = Lens.lens (\ListTableMetadataResponse' {httpStatus} -> httpStatus) (\s@ListTableMetadataResponse' {} a -> s {httpStatus = a} :: ListTableMetadataResponse)

instance Prelude.NFData ListTableMetadataResponse where
  rnf ListTableMetadataResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf tableMetadataList `Prelude.seq`
        Prelude.rnf httpStatus
