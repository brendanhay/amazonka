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
-- Module      : Amazonka.SageMaker.ListAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the aliases of a specified image or image version.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_alias,
    listAliases_maxResults,
    listAliases_nextToken,
    listAliases_version,
    listAliases_imageName,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_nextToken,
    listAliasesResponse_sageMakerImageVersionAliases,
    listAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | The alias of the image version.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of aliases to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous call to @ListAliases@ didn\'t return the full set of
    -- aliases, the call returns a token for retrieving the next set of
    -- aliases.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the image. If image version is not specified, the aliases
    -- of all versions of the image are listed.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the image.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'listAliases_alias' - The alias of the image version.
--
-- 'maxResults', 'listAliases_maxResults' - The maximum number of aliases to return.
--
-- 'nextToken', 'listAliases_nextToken' - If the previous call to @ListAliases@ didn\'t return the full set of
-- aliases, the call returns a token for retrieving the next set of
-- aliases.
--
-- 'version', 'listAliases_version' - The version of the image. If image version is not specified, the aliases
-- of all versions of the image are listed.
--
-- 'imageName', 'listAliases_imageName' - The name of the image.
newListAliases ::
  -- | 'imageName'
  Prelude.Text ->
  ListAliases
newListAliases pImageName_ =
  ListAliases'
    { alias = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      version = Prelude.Nothing,
      imageName = pImageName_
    }

-- | The alias of the image version.
listAliases_alias :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_alias = Lens.lens (\ListAliases' {alias} -> alias) (\s@ListAliases' {} a -> s {alias = a} :: ListAliases)

-- | The maximum number of aliases to return.
listAliases_maxResults :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_maxResults = Lens.lens (\ListAliases' {maxResults} -> maxResults) (\s@ListAliases' {} a -> s {maxResults = a} :: ListAliases)

-- | If the previous call to @ListAliases@ didn\'t return the full set of
-- aliases, the call returns a token for retrieving the next set of
-- aliases.
listAliases_nextToken :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_nextToken = Lens.lens (\ListAliases' {nextToken} -> nextToken) (\s@ListAliases' {} a -> s {nextToken = a} :: ListAliases)

-- | The version of the image. If image version is not specified, the aliases
-- of all versions of the image are listed.
listAliases_version :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_version = Lens.lens (\ListAliases' {version} -> version) (\s@ListAliases' {} a -> s {version = a} :: ListAliases)

-- | The name of the image.
listAliases_imageName :: Lens.Lens' ListAliases Prelude.Text
listAliases_imageName = Lens.lens (\ListAliases' {imageName} -> imageName) (\s@ListAliases' {} a -> s {imageName = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_sageMakerImageVersionAliases
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAliases_nextToken
          Lens..~ rs
          Lens.^? listAliasesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SageMakerImageVersionAliases"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases where
  hashWithSalt _salt ListAliases' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` imageName

instance Prelude.NFData ListAliases where
  rnf ListAliases' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf imageName

instance Data.ToHeaders ListAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListAliases" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Alias" Data..=) Prelude.<$> alias,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("ImageName" Data..= imageName)
          ]
      )

instance Data.ToPath ListAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A token for getting the next set of aliases, if more aliases exist.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of SageMaker image version aliases.
    sageMakerImageVersionAliases :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAliasesResponse_nextToken' - A token for getting the next set of aliases, if more aliases exist.
--
-- 'sageMakerImageVersionAliases', 'listAliasesResponse_sageMakerImageVersionAliases' - A list of SageMaker image version aliases.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextToken = Prelude.Nothing,
      sageMakerImageVersionAliases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of aliases, if more aliases exist.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | A list of SageMaker image version aliases.
listAliasesResponse_sageMakerImageVersionAliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [Prelude.Text])
listAliasesResponse_sageMakerImageVersionAliases = Lens.lens (\ListAliasesResponse' {sageMakerImageVersionAliases} -> sageMakerImageVersionAliases) (\s@ListAliasesResponse' {} a -> s {sageMakerImageVersionAliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse where
  rnf ListAliasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sageMakerImageVersionAliases
      `Prelude.seq` Prelude.rnf httpStatus
