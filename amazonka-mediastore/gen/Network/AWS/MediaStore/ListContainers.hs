{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaStore.ListContainers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.MediaStore.ListContainers
  ( -- * Creating a Request
    ListContainers (..),
    newListContainers,

    -- * Request Lenses
    listContainers_nextToken,
    listContainers_maxResults,

    -- * Destructuring the Response
    ListContainersResponse (..),
    newListContainersResponse,

    -- * Response Lenses
    listContainersResponse_nextToken,
    listContainersResponse_httpStatus,
    listContainersResponse_containers,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListContainers' smart constructor.
data ListContainers = ListContainers'
  { -- | Only if you used @MaxResults@ in the first command, enter the token
    -- (which was included in the previous response) to obtain the next set of
    -- containers. This token is included in a response only if there actually
    -- are more containers to list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Enter the maximum number of containers in the response. Use from 1 to
    -- 255 characters.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListContainers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContainers_nextToken' - Only if you used @MaxResults@ in the first command, enter the token
-- (which was included in the previous response) to obtain the next set of
-- containers. This token is included in a response only if there actually
-- are more containers to list.
--
-- 'maxResults', 'listContainers_maxResults' - Enter the maximum number of containers in the response. Use from 1 to
-- 255 characters.
newListContainers ::
  ListContainers
newListContainers =
  ListContainers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Only if you used @MaxResults@ in the first command, enter the token
-- (which was included in the previous response) to obtain the next set of
-- containers. This token is included in a response only if there actually
-- are more containers to list.
listContainers_nextToken :: Lens.Lens' ListContainers (Prelude.Maybe Prelude.Text)
listContainers_nextToken = Lens.lens (\ListContainers' {nextToken} -> nextToken) (\s@ListContainers' {} a -> s {nextToken = a} :: ListContainers)

-- | Enter the maximum number of containers in the response. Use from 1 to
-- 255 characters.
listContainers_maxResults :: Lens.Lens' ListContainers (Prelude.Maybe Prelude.Natural)
listContainers_maxResults = Lens.lens (\ListContainers' {maxResults} -> maxResults) (\s@ListContainers' {} a -> s {maxResults = a} :: ListContainers)

instance Pager.AWSPager ListContainers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listContainersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listContainersResponse_containers) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listContainers_nextToken
          Lens..~ rs
          Lens.^? listContainersResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListContainers where
  type Rs ListContainers = ListContainersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContainersResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "Containers"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListContainers

instance Prelude.NFData ListContainers

instance Prelude.ToHeaders ListContainers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.ListContainers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListContainers where
  toJSON ListContainers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListContainers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListContainers where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listContainersResponse_containers = Lens.lens (\ListContainersResponse' {containers} -> containers) (\s@ListContainersResponse' {} a -> s {containers = a} :: ListContainersResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListContainersResponse
