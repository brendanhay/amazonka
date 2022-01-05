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
-- Module      : Amazonka.DevOpsGuru.GetResourceCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns lists AWS resources that are of the specified resource
-- collection type. The one type of AWS resource collection supported is
-- AWS CloudFormation stacks. DevOps Guru can be configured to analyze only
-- the AWS resources that are defined in the stacks. You can specify up to
-- 500 AWS CloudFormation stacks.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.GetResourceCollection
  ( -- * Creating a Request
    GetResourceCollection (..),
    newGetResourceCollection,

    -- * Request Lenses
    getResourceCollection_nextToken,
    getResourceCollection_resourceCollectionType,

    -- * Destructuring the Response
    GetResourceCollectionResponse (..),
    newGetResourceCollectionResponse,

    -- * Response Lenses
    getResourceCollectionResponse_resourceCollection,
    getResourceCollectionResponse_nextToken,
    getResourceCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceCollection' smart constructor.
data GetResourceCollection = GetResourceCollection'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS resource collections to return. The one valid value is
    -- @CLOUD_FORMATION@ for AWS CloudFormation stacks.
    resourceCollectionType :: ResourceCollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceCollection_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'resourceCollectionType', 'getResourceCollection_resourceCollectionType' - The type of AWS resource collections to return. The one valid value is
-- @CLOUD_FORMATION@ for AWS CloudFormation stacks.
newGetResourceCollection ::
  -- | 'resourceCollectionType'
  ResourceCollectionType ->
  GetResourceCollection
newGetResourceCollection pResourceCollectionType_ =
  GetResourceCollection'
    { nextToken = Prelude.Nothing,
      resourceCollectionType = pResourceCollectionType_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
getResourceCollection_nextToken :: Lens.Lens' GetResourceCollection (Prelude.Maybe Prelude.Text)
getResourceCollection_nextToken = Lens.lens (\GetResourceCollection' {nextToken} -> nextToken) (\s@GetResourceCollection' {} a -> s {nextToken = a} :: GetResourceCollection)

-- | The type of AWS resource collections to return. The one valid value is
-- @CLOUD_FORMATION@ for AWS CloudFormation stacks.
getResourceCollection_resourceCollectionType :: Lens.Lens' GetResourceCollection ResourceCollectionType
getResourceCollection_resourceCollectionType = Lens.lens (\GetResourceCollection' {resourceCollectionType} -> resourceCollectionType) (\s@GetResourceCollection' {} a -> s {resourceCollectionType = a} :: GetResourceCollection)

instance Core.AWSPager GetResourceCollection where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceCollectionResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceCollectionResponse_resourceCollection
              Prelude.. Lens._Just
              Prelude.. resourceCollectionFilter_cloudFormation
              Prelude.. Lens._Just
              Prelude.. cloudFormationCollectionFilter_stackNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourceCollection_nextToken
          Lens..~ rs
          Lens.^? getResourceCollectionResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourceCollection where
  type
    AWSResponse GetResourceCollection =
      GetResourceCollectionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceCollectionResponse'
            Prelude.<$> (x Core..?> "ResourceCollection")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceCollection where
  hashWithSalt _salt GetResourceCollection' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceCollectionType

instance Prelude.NFData GetResourceCollection where
  rnf GetResourceCollection' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceCollectionType

instance Core.ToHeaders GetResourceCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResourceCollection where
  toPath GetResourceCollection' {..} =
    Prelude.mconcat
      [ "/resource-collections/",
        Core.toBS resourceCollectionType
      ]

instance Core.ToQuery GetResourceCollection where
  toQuery GetResourceCollection' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetResourceCollectionResponse' smart constructor.
data GetResourceCollectionResponse = GetResourceCollectionResponse'
  { -- | The requested list of AWS resource collections. The one type of AWS
    -- resource collection supported is AWS CloudFormation stacks. DevOps Guru
    -- can be configured to analyze only the AWS resources that are defined in
    -- the stacks. You can specify up to 500 AWS CloudFormation stacks.
    resourceCollection :: Prelude.Maybe ResourceCollectionFilter,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceCollection', 'getResourceCollectionResponse_resourceCollection' - The requested list of AWS resource collections. The one type of AWS
-- resource collection supported is AWS CloudFormation stacks. DevOps Guru
-- can be configured to analyze only the AWS resources that are defined in
-- the stacks. You can specify up to 500 AWS CloudFormation stacks.
--
-- 'nextToken', 'getResourceCollectionResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'getResourceCollectionResponse_httpStatus' - The response's http status code.
newGetResourceCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceCollectionResponse
newGetResourceCollectionResponse pHttpStatus_ =
  GetResourceCollectionResponse'
    { resourceCollection =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested list of AWS resource collections. The one type of AWS
-- resource collection supported is AWS CloudFormation stacks. DevOps Guru
-- can be configured to analyze only the AWS resources that are defined in
-- the stacks. You can specify up to 500 AWS CloudFormation stacks.
getResourceCollectionResponse_resourceCollection :: Lens.Lens' GetResourceCollectionResponse (Prelude.Maybe ResourceCollectionFilter)
getResourceCollectionResponse_resourceCollection = Lens.lens (\GetResourceCollectionResponse' {resourceCollection} -> resourceCollection) (\s@GetResourceCollectionResponse' {} a -> s {resourceCollection = a} :: GetResourceCollectionResponse)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
getResourceCollectionResponse_nextToken :: Lens.Lens' GetResourceCollectionResponse (Prelude.Maybe Prelude.Text)
getResourceCollectionResponse_nextToken = Lens.lens (\GetResourceCollectionResponse' {nextToken} -> nextToken) (\s@GetResourceCollectionResponse' {} a -> s {nextToken = a} :: GetResourceCollectionResponse)

-- | The response's http status code.
getResourceCollectionResponse_httpStatus :: Lens.Lens' GetResourceCollectionResponse Prelude.Int
getResourceCollectionResponse_httpStatus = Lens.lens (\GetResourceCollectionResponse' {httpStatus} -> httpStatus) (\s@GetResourceCollectionResponse' {} a -> s {httpStatus = a} :: GetResourceCollectionResponse)

instance Prelude.NFData GetResourceCollectionResponse where
  rnf GetResourceCollectionResponse' {..} =
    Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
