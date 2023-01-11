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
-- Module      : Amazonka.ECS.ListServicesByNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists all of the services that are associated with a
-- Cloud Map namespace. This list might include services in different
-- clusters. In contrast, @ListServices@ can only list services in one
-- cluster at a time. If you need to filter the list of services in a
-- single cluster by various parameters, use @ListServices@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.ECS.ListServicesByNamespace
  ( -- * Creating a Request
    ListServicesByNamespace (..),
    newListServicesByNamespace,

    -- * Request Lenses
    listServicesByNamespace_maxResults,
    listServicesByNamespace_nextToken,
    listServicesByNamespace_namespace,

    -- * Destructuring the Response
    ListServicesByNamespaceResponse (..),
    newListServicesByNamespaceResponse,

    -- * Response Lenses
    listServicesByNamespaceResponse_nextToken,
    listServicesByNamespaceResponse_serviceArns,
    listServicesByNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServicesByNamespace' smart constructor.
data ListServicesByNamespace = ListServicesByNamespace'
  { -- | The maximum number of service results that @ListServicesByNamespace@
    -- returns in paginated output. When this parameter is used,
    -- @ListServicesByNamespace@ only returns @maxResults@ results in a single
    -- page along with a @nextToken@ response element. The remaining results of
    -- the initial request can be seen by sending another
    -- @ListServicesByNamespace@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter isn\'t used, then
    -- @ListServicesByNamespace@ returns up to 10 results and a @nextToken@
    -- value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @nextToken@ value that\'s returned from a @ListServicesByNamespace@
    -- request. It indicates that more results are available to fulfill the
    -- request and further calls are needed. If @maxResults@ is returned, it is
    -- possible the number of results is less than @maxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
    -- namespace to list the services in.
    --
    -- Tasks that run in a namespace can use short names to connect to services
    -- in the namespace. Tasks can connect to services across all of the
    -- clusters in the namespace. Tasks connect through a managed proxy
    -- container that collects logs and metrics for increased visibility. Only
    -- the tasks that Amazon ECS services create are supported with Service
    -- Connect. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicesByNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listServicesByNamespace_maxResults' - The maximum number of service results that @ListServicesByNamespace@
-- returns in paginated output. When this parameter is used,
-- @ListServicesByNamespace@ only returns @maxResults@ results in a single
-- page along with a @nextToken@ response element. The remaining results of
-- the initial request can be seen by sending another
-- @ListServicesByNamespace@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListServicesByNamespace@ returns up to 10 results and a @nextToken@
-- value if applicable.
--
-- 'nextToken', 'listServicesByNamespace_nextToken' - The @nextToken@ value that\'s returned from a @ListServicesByNamespace@
-- request. It indicates that more results are available to fulfill the
-- request and further calls are needed. If @maxResults@ is returned, it is
-- possible the number of results is less than @maxResults@.
--
-- 'namespace', 'listServicesByNamespace_namespace' - The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace to list the services in.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
newListServicesByNamespace ::
  -- | 'namespace'
  Prelude.Text ->
  ListServicesByNamespace
newListServicesByNamespace pNamespace_ =
  ListServicesByNamespace'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      namespace = pNamespace_
    }

-- | The maximum number of service results that @ListServicesByNamespace@
-- returns in paginated output. When this parameter is used,
-- @ListServicesByNamespace@ only returns @maxResults@ results in a single
-- page along with a @nextToken@ response element. The remaining results of
-- the initial request can be seen by sending another
-- @ListServicesByNamespace@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListServicesByNamespace@ returns up to 10 results and a @nextToken@
-- value if applicable.
listServicesByNamespace_maxResults :: Lens.Lens' ListServicesByNamespace (Prelude.Maybe Prelude.Int)
listServicesByNamespace_maxResults = Lens.lens (\ListServicesByNamespace' {maxResults} -> maxResults) (\s@ListServicesByNamespace' {} a -> s {maxResults = a} :: ListServicesByNamespace)

-- | The @nextToken@ value that\'s returned from a @ListServicesByNamespace@
-- request. It indicates that more results are available to fulfill the
-- request and further calls are needed. If @maxResults@ is returned, it is
-- possible the number of results is less than @maxResults@.
listServicesByNamespace_nextToken :: Lens.Lens' ListServicesByNamespace (Prelude.Maybe Prelude.Text)
listServicesByNamespace_nextToken = Lens.lens (\ListServicesByNamespace' {nextToken} -> nextToken) (\s@ListServicesByNamespace' {} a -> s {nextToken = a} :: ListServicesByNamespace)

-- | The namespace name or full Amazon Resource Name (ARN) of the Cloud Map
-- namespace to list the services in.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
listServicesByNamespace_namespace :: Lens.Lens' ListServicesByNamespace Prelude.Text
listServicesByNamespace_namespace = Lens.lens (\ListServicesByNamespace' {namespace} -> namespace) (\s@ListServicesByNamespace' {} a -> s {namespace = a} :: ListServicesByNamespace)

instance Core.AWSPager ListServicesByNamespace where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicesByNamespaceResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServicesByNamespaceResponse_serviceArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServicesByNamespace_nextToken
          Lens..~ rs
          Lens.^? listServicesByNamespaceResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListServicesByNamespace where
  type
    AWSResponse ListServicesByNamespace =
      ListServicesByNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicesByNamespaceResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "serviceArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServicesByNamespace where
  hashWithSalt _salt ListServicesByNamespace' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ListServicesByNamespace where
  rnf ListServicesByNamespace' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders ListServicesByNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.ListServicesByNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServicesByNamespace where
  toJSON ListServicesByNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("namespace" Data..= namespace)
          ]
      )

instance Data.ToPath ListServicesByNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServicesByNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicesByNamespaceResponse' smart constructor.
data ListServicesByNamespaceResponse = ListServicesByNamespaceResponse'
  { -- | The @nextToken@ value to include in a future @ListServicesByNamespace@
    -- request. When the results of a @ListServicesByNamespace@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. When there are no more results to return, this value is @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of full ARN entries for each service that\'s associated with
    -- the specified namespace.
    serviceArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicesByNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicesByNamespaceResponse_nextToken' - The @nextToken@ value to include in a future @ListServicesByNamespace@
-- request. When the results of a @ListServicesByNamespace@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. When there are no more results to return, this value is @null@.
--
-- 'serviceArns', 'listServicesByNamespaceResponse_serviceArns' - The list of full ARN entries for each service that\'s associated with
-- the specified namespace.
--
-- 'httpStatus', 'listServicesByNamespaceResponse_httpStatus' - The response's http status code.
newListServicesByNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServicesByNamespaceResponse
newListServicesByNamespaceResponse pHttpStatus_ =
  ListServicesByNamespaceResponse'
    { nextToken =
        Prelude.Nothing,
      serviceArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListServicesByNamespace@
-- request. When the results of a @ListServicesByNamespace@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. When there are no more results to return, this value is @null@.
listServicesByNamespaceResponse_nextToken :: Lens.Lens' ListServicesByNamespaceResponse (Prelude.Maybe Prelude.Text)
listServicesByNamespaceResponse_nextToken = Lens.lens (\ListServicesByNamespaceResponse' {nextToken} -> nextToken) (\s@ListServicesByNamespaceResponse' {} a -> s {nextToken = a} :: ListServicesByNamespaceResponse)

-- | The list of full ARN entries for each service that\'s associated with
-- the specified namespace.
listServicesByNamespaceResponse_serviceArns :: Lens.Lens' ListServicesByNamespaceResponse (Prelude.Maybe [Prelude.Text])
listServicesByNamespaceResponse_serviceArns = Lens.lens (\ListServicesByNamespaceResponse' {serviceArns} -> serviceArns) (\s@ListServicesByNamespaceResponse' {} a -> s {serviceArns = a} :: ListServicesByNamespaceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServicesByNamespaceResponse_httpStatus :: Lens.Lens' ListServicesByNamespaceResponse Prelude.Int
listServicesByNamespaceResponse_httpStatus = Lens.lens (\ListServicesByNamespaceResponse' {httpStatus} -> httpStatus) (\s@ListServicesByNamespaceResponse' {} a -> s {httpStatus = a} :: ListServicesByNamespaceResponse)

instance
  Prelude.NFData
    ListServicesByNamespaceResponse
  where
  rnf ListServicesByNamespaceResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceArns
      `Prelude.seq` Prelude.rnf httpStatus
