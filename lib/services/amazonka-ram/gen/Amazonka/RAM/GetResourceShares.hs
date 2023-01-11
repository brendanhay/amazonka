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
-- Module      : Amazonka.RAM.GetResourceShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about the resource shares that you own or that are
-- shared with you.
--
-- This operation returns paginated results.
module Amazonka.RAM.GetResourceShares
  ( -- * Creating a Request
    GetResourceShares (..),
    newGetResourceShares,

    -- * Request Lenses
    getResourceShares_maxResults,
    getResourceShares_name,
    getResourceShares_nextToken,
    getResourceShares_permissionArn,
    getResourceShares_resourceShareArns,
    getResourceShares_resourceShareStatus,
    getResourceShares_tagFilters,
    getResourceShares_resourceOwner,

    -- * Destructuring the Response
    GetResourceSharesResponse (..),
    newGetResourceSharesResponse,

    -- * Response Lenses
    getResourceSharesResponse_nextToken,
    getResourceSharesResponse_resourceShares,
    getResourceSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceShares' smart constructor.
data GetResourceShares = GetResourceShares'
  { -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the name of an individual resource share that you want to
    -- retrieve details about.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to retrieve details of only those resource
    -- shares that use the RAM permission with this
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
    permissionArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- of individual resource shares that you want information about.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies that you want to retrieve details of only those resource
    -- shares that have this status.
    resourceShareStatus :: Prelude.Maybe ResourceShareStatus,
    -- | Specifies that you want to retrieve details of only those resource
    -- shares that match the specified tag keys and values.
    tagFilters :: Prelude.Maybe [TagFilter],
    -- | Specifies that you want to retrieve details of only those resource
    -- shares that match the following:
    --
    -- -   __@SELF@__ – resource shares that your account shares with other
    --     accounts
    --
    -- -   __@OTHER-ACCOUNTS@__ – resource shares that other accounts share
    --     with your account
    resourceOwner :: ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getResourceShares_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'name', 'getResourceShares_name' - Specifies the name of an individual resource share that you want to
-- retrieve details about.
--
-- 'nextToken', 'getResourceShares_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'permissionArn', 'getResourceShares_permissionArn' - Specifies that you want to retrieve details of only those resource
-- shares that use the RAM permission with this
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
--
-- 'resourceShareArns', 'getResourceShares_resourceShareArns' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of individual resource shares that you want information about.
--
-- 'resourceShareStatus', 'getResourceShares_resourceShareStatus' - Specifies that you want to retrieve details of only those resource
-- shares that have this status.
--
-- 'tagFilters', 'getResourceShares_tagFilters' - Specifies that you want to retrieve details of only those resource
-- shares that match the specified tag keys and values.
--
-- 'resourceOwner', 'getResourceShares_resourceOwner' - Specifies that you want to retrieve details of only those resource
-- shares that match the following:
--
-- -   __@SELF@__ – resource shares that your account shares with other
--     accounts
--
-- -   __@OTHER-ACCOUNTS@__ – resource shares that other accounts share
--     with your account
newGetResourceShares ::
  -- | 'resourceOwner'
  ResourceOwner ->
  GetResourceShares
newGetResourceShares pResourceOwner_ =
  GetResourceShares'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      permissionArn = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      resourceShareStatus = Prelude.Nothing,
      tagFilters = Prelude.Nothing,
      resourceOwner = pResourceOwner_
    }

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
getResourceShares_maxResults :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Natural)
getResourceShares_maxResults = Lens.lens (\GetResourceShares' {maxResults} -> maxResults) (\s@GetResourceShares' {} a -> s {maxResults = a} :: GetResourceShares)

-- | Specifies the name of an individual resource share that you want to
-- retrieve details about.
getResourceShares_name :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_name = Lens.lens (\GetResourceShares' {name} -> name) (\s@GetResourceShares' {} a -> s {name = a} :: GetResourceShares)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
getResourceShares_nextToken :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_nextToken = Lens.lens (\GetResourceShares' {nextToken} -> nextToken) (\s@GetResourceShares' {} a -> s {nextToken = a} :: GetResourceShares)

-- | Specifies that you want to retrieve details of only those resource
-- shares that use the RAM permission with this
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
getResourceShares_permissionArn :: Lens.Lens' GetResourceShares (Prelude.Maybe Prelude.Text)
getResourceShares_permissionArn = Lens.lens (\GetResourceShares' {permissionArn} -> permissionArn) (\s@GetResourceShares' {} a -> s {permissionArn = a} :: GetResourceShares)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of individual resource shares that you want information about.
getResourceShares_resourceShareArns :: Lens.Lens' GetResourceShares (Prelude.Maybe [Prelude.Text])
getResourceShares_resourceShareArns = Lens.lens (\GetResourceShares' {resourceShareArns} -> resourceShareArns) (\s@GetResourceShares' {} a -> s {resourceShareArns = a} :: GetResourceShares) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want to retrieve details of only those resource
-- shares that have this status.
getResourceShares_resourceShareStatus :: Lens.Lens' GetResourceShares (Prelude.Maybe ResourceShareStatus)
getResourceShares_resourceShareStatus = Lens.lens (\GetResourceShares' {resourceShareStatus} -> resourceShareStatus) (\s@GetResourceShares' {} a -> s {resourceShareStatus = a} :: GetResourceShares)

-- | Specifies that you want to retrieve details of only those resource
-- shares that match the specified tag keys and values.
getResourceShares_tagFilters :: Lens.Lens' GetResourceShares (Prelude.Maybe [TagFilter])
getResourceShares_tagFilters = Lens.lens (\GetResourceShares' {tagFilters} -> tagFilters) (\s@GetResourceShares' {} a -> s {tagFilters = a} :: GetResourceShares) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want to retrieve details of only those resource
-- shares that match the following:
--
-- -   __@SELF@__ – resource shares that your account shares with other
--     accounts
--
-- -   __@OTHER-ACCOUNTS@__ – resource shares that other accounts share
--     with your account
getResourceShares_resourceOwner :: Lens.Lens' GetResourceShares ResourceOwner
getResourceShares_resourceOwner = Lens.lens (\GetResourceShares' {resourceOwner} -> resourceOwner) (\s@GetResourceShares' {} a -> s {resourceOwner = a} :: GetResourceShares)

instance Core.AWSPager GetResourceShares where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceSharesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceSharesResponse_resourceShares
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourceShares_nextToken
          Lens..~ rs
          Lens.^? getResourceSharesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourceShares where
  type
    AWSResponse GetResourceShares =
      GetResourceSharesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceSharesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resourceShares" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceShares where
  hashWithSalt _salt GetResourceShares' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` permissionArn
      `Prelude.hashWithSalt` resourceShareArns
      `Prelude.hashWithSalt` resourceShareStatus
      `Prelude.hashWithSalt` tagFilters
      `Prelude.hashWithSalt` resourceOwner

instance Prelude.NFData GetResourceShares where
  rnf GetResourceShares' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf resourceShareArns
      `Prelude.seq` Prelude.rnf resourceShareStatus
      `Prelude.seq` Prelude.rnf tagFilters
      `Prelude.seq` Prelude.rnf resourceOwner

instance Data.ToHeaders GetResourceShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceShares where
  toJSON GetResourceShares' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("permissionArn" Data..=) Prelude.<$> permissionArn,
            ("resourceShareArns" Data..=)
              Prelude.<$> resourceShareArns,
            ("resourceShareStatus" Data..=)
              Prelude.<$> resourceShareStatus,
            ("tagFilters" Data..=) Prelude.<$> tagFilters,
            Prelude.Just
              ("resourceOwner" Data..= resourceOwner)
          ]
      )

instance Data.ToPath GetResourceShares where
  toPath = Prelude.const "/getresourceshares"

instance Data.ToQuery GetResourceShares where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceSharesResponse' smart constructor.
data GetResourceSharesResponse = GetResourceSharesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain the information about the resource
    -- shares.
    resourceShares :: Prelude.Maybe [ResourceShare],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceSharesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'resourceShares', 'getResourceSharesResponse_resourceShares' - An array of objects that contain the information about the resource
-- shares.
--
-- 'httpStatus', 'getResourceSharesResponse_httpStatus' - The response's http status code.
newGetResourceSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceSharesResponse
newGetResourceSharesResponse pHttpStatus_ =
  GetResourceSharesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceShares = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
getResourceSharesResponse_nextToken :: Lens.Lens' GetResourceSharesResponse (Prelude.Maybe Prelude.Text)
getResourceSharesResponse_nextToken = Lens.lens (\GetResourceSharesResponse' {nextToken} -> nextToken) (\s@GetResourceSharesResponse' {} a -> s {nextToken = a} :: GetResourceSharesResponse)

-- | An array of objects that contain the information about the resource
-- shares.
getResourceSharesResponse_resourceShares :: Lens.Lens' GetResourceSharesResponse (Prelude.Maybe [ResourceShare])
getResourceSharesResponse_resourceShares = Lens.lens (\GetResourceSharesResponse' {resourceShares} -> resourceShares) (\s@GetResourceSharesResponse' {} a -> s {resourceShares = a} :: GetResourceSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceSharesResponse_httpStatus :: Lens.Lens' GetResourceSharesResponse Prelude.Int
getResourceSharesResponse_httpStatus = Lens.lens (\GetResourceSharesResponse' {httpStatus} -> httpStatus) (\s@GetResourceSharesResponse' {} a -> s {httpStatus = a} :: GetResourceSharesResponse)

instance Prelude.NFData GetResourceSharesResponse where
  rnf GetResourceSharesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShares
      `Prelude.seq` Prelude.rnf httpStatus
