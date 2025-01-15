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
-- Module      : Amazonka.MediaStore.ListContainers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of all containers in AWS Elemental MediaStore.
--
-- You can query to receive all the containers in one response. Or you can
-- include the @MaxResults@ parameter to receive a limited number of
-- containers in each response. In this case, the response includes a
-- token. To get the next set of containers, send the command again, this
-- time with the @NextToken@ parameter (with the returned token as its
-- value). The next set of responses appears, with a token if there are
-- still more containers to receive.
--
-- See also DescribeContainer, which gets the properties of one container.
--
-- This operation returns paginated results.
module Amazonka.MediaStore.ListContainers
  ( -- * Creating a Request
    ListContainers (..),
    newListContainers,

    -- * Request Lenses
    listContainers_maxResults,
    listContainers_nextToken,

    -- * Destructuring the Response
    ListContainersResponse (..),
    newListContainersResponse,

    -- * Response Lenses
    listContainersResponse_nextToken,
    listContainersResponse_httpStatus,
    listContainersResponse_containers,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContainers' smart constructor.
data ListContainers = ListContainers'
  { -- | Enter the maximum number of containers in the response. Use from 1 to
    -- 255 characters.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Only if you used @MaxResults@ in the first command, enter the token
    -- (which was included in the previous response) to obtain the next set of
    -- containers. This token is included in a response only if there actually
    -- are more containers to list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContainers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listContainers_maxResults' - Enter the maximum number of containers in the response. Use from 1 to
-- 255 characters.
--
-- 'nextToken', 'listContainers_nextToken' - Only if you used @MaxResults@ in the first command, enter the token
-- (which was included in the previous response) to obtain the next set of
-- containers. This token is included in a response only if there actually
-- are more containers to list.
newListContainers ::
  ListContainers
newListContainers =
  ListContainers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Enter the maximum number of containers in the response. Use from 1 to
-- 255 characters.
listContainers_maxResults :: Lens.Lens' ListContainers (Prelude.Maybe Prelude.Natural)
listContainers_maxResults = Lens.lens (\ListContainers' {maxResults} -> maxResults) (\s@ListContainers' {} a -> s {maxResults = a} :: ListContainers)

-- | Only if you used @MaxResults@ in the first command, enter the token
-- (which was included in the previous response) to obtain the next set of
-- containers. This token is included in a response only if there actually
-- are more containers to list.
listContainers_nextToken :: Lens.Lens' ListContainers (Prelude.Maybe Prelude.Text)
listContainers_nextToken = Lens.lens (\ListContainers' {nextToken} -> nextToken) (\s@ListContainers' {} a -> s {nextToken = a} :: ListContainers)

instance Core.AWSPager ListContainers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContainersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listContainersResponse_containers) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listContainers_nextToken
              Lens..~ rs
              Lens.^? listContainersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListContainers where
  type
    AWSResponse ListContainers =
      ListContainersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContainersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Containers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListContainers where
  hashWithSalt _salt ListContainers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListContainers where
  rnf ListContainers' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListContainers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.ListContainers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListContainers where
  toJSON ListContainers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListContainers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListContainers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContainersResponse' smart constructor.
data ListContainersResponse = ListContainersResponse'
  { -- | @NextToken@ is the token to use in the next call to @ListContainers@.
    -- This token is returned only if you included the @MaxResults@ tag in the
    -- original command, and only if there are still containers to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The names of the containers.
    containers :: [Container]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContainersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContainersResponse_nextToken' - @NextToken@ is the token to use in the next call to @ListContainers@.
-- This token is returned only if you included the @MaxResults@ tag in the
-- original command, and only if there are still containers to return.
--
-- 'httpStatus', 'listContainersResponse_httpStatus' - The response's http status code.
--
-- 'containers', 'listContainersResponse_containers' - The names of the containers.
newListContainersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContainersResponse
newListContainersResponse pHttpStatus_ =
  ListContainersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      containers = Prelude.mempty
    }

-- | @NextToken@ is the token to use in the next call to @ListContainers@.
-- This token is returned only if you included the @MaxResults@ tag in the
-- original command, and only if there are still containers to return.
listContainersResponse_nextToken :: Lens.Lens' ListContainersResponse (Prelude.Maybe Prelude.Text)
listContainersResponse_nextToken = Lens.lens (\ListContainersResponse' {nextToken} -> nextToken) (\s@ListContainersResponse' {} a -> s {nextToken = a} :: ListContainersResponse)

-- | The response's http status code.
listContainersResponse_httpStatus :: Lens.Lens' ListContainersResponse Prelude.Int
listContainersResponse_httpStatus = Lens.lens (\ListContainersResponse' {httpStatus} -> httpStatus) (\s@ListContainersResponse' {} a -> s {httpStatus = a} :: ListContainersResponse)

-- | The names of the containers.
listContainersResponse_containers :: Lens.Lens' ListContainersResponse [Container]
listContainersResponse_containers = Lens.lens (\ListContainersResponse' {containers} -> containers) (\s@ListContainersResponse' {} a -> s {containers = a} :: ListContainersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListContainersResponse where
  rnf ListContainersResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf containers
