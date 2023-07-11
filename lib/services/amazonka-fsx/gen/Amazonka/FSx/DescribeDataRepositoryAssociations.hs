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
-- Module      : Amazonka.FSx.DescribeDataRepositoryAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of specific Amazon FSx for Lustre or Amazon File
-- Cache data repository associations, if one or more @AssociationIds@
-- values are provided in the request, or if filters are used in the
-- request. Data repository associations are supported only for Amazon FSx
-- for Lustre file systems with the @Persistent_2@ deployment type and for
-- Amazon File Cache resources.
--
-- You can use filters to narrow the response to include just data
-- repository associations for specific file systems (use the
-- @file-system-id@ filter with the ID of the file system) or caches (use
-- the @file-cache-id@ filter with the ID of the cache), or data repository
-- associations for a specific repository type (use the
-- @data-repository-type@ filter with a value of @S3@ or @NFS@). If you
-- don\'t use filters, the response returns all data repository
-- associations owned by your Amazon Web Services account in the Amazon Web
-- Services Region of the endpoint that you\'re calling.
--
-- When retrieving all data repository associations, you can paginate the
-- response by using the optional @MaxResults@ parameter to limit the
-- number of data repository associations returned in a response. If more
-- data repository associations remain, a @NextToken@ value is returned in
-- the response. In this case, send a later request with the @NextToken@
-- request parameter set to the value of @NextToken@ from the last
-- response.
module Amazonka.FSx.DescribeDataRepositoryAssociations
  ( -- * Creating a Request
    DescribeDataRepositoryAssociations (..),
    newDescribeDataRepositoryAssociations,

    -- * Request Lenses
    describeDataRepositoryAssociations_associationIds,
    describeDataRepositoryAssociations_filters,
    describeDataRepositoryAssociations_maxResults,
    describeDataRepositoryAssociations_nextToken,

    -- * Destructuring the Response
    DescribeDataRepositoryAssociationsResponse (..),
    newDescribeDataRepositoryAssociationsResponse,

    -- * Response Lenses
    describeDataRepositoryAssociationsResponse_associations,
    describeDataRepositoryAssociationsResponse_nextToken,
    describeDataRepositoryAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataRepositoryAssociations' smart constructor.
data DescribeDataRepositoryAssociations = DescribeDataRepositoryAssociations'
  { -- | IDs of the data repository associations whose descriptions you want to
    -- retrieve (String).
    associationIds :: Prelude.Maybe [Prelude.Text],
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of resources to return in the response. This value
    -- must be an integer greater than zero.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataRepositoryAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationIds', 'describeDataRepositoryAssociations_associationIds' - IDs of the data repository associations whose descriptions you want to
-- retrieve (String).
--
-- 'filters', 'describeDataRepositoryAssociations_filters' - Undocumented member.
--
-- 'maxResults', 'describeDataRepositoryAssociations_maxResults' - The maximum number of resources to return in the response. This value
-- must be an integer greater than zero.
--
-- 'nextToken', 'describeDataRepositoryAssociations_nextToken' - Undocumented member.
newDescribeDataRepositoryAssociations ::
  DescribeDataRepositoryAssociations
newDescribeDataRepositoryAssociations =
  DescribeDataRepositoryAssociations'
    { associationIds =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | IDs of the data repository associations whose descriptions you want to
-- retrieve (String).
describeDataRepositoryAssociations_associationIds :: Lens.Lens' DescribeDataRepositoryAssociations (Prelude.Maybe [Prelude.Text])
describeDataRepositoryAssociations_associationIds = Lens.lens (\DescribeDataRepositoryAssociations' {associationIds} -> associationIds) (\s@DescribeDataRepositoryAssociations' {} a -> s {associationIds = a} :: DescribeDataRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDataRepositoryAssociations_filters :: Lens.Lens' DescribeDataRepositoryAssociations (Prelude.Maybe [Filter])
describeDataRepositoryAssociations_filters = Lens.lens (\DescribeDataRepositoryAssociations' {filters} -> filters) (\s@DescribeDataRepositoryAssociations' {} a -> s {filters = a} :: DescribeDataRepositoryAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of resources to return in the response. This value
-- must be an integer greater than zero.
describeDataRepositoryAssociations_maxResults :: Lens.Lens' DescribeDataRepositoryAssociations (Prelude.Maybe Prelude.Natural)
describeDataRepositoryAssociations_maxResults = Lens.lens (\DescribeDataRepositoryAssociations' {maxResults} -> maxResults) (\s@DescribeDataRepositoryAssociations' {} a -> s {maxResults = a} :: DescribeDataRepositoryAssociations)

-- | Undocumented member.
describeDataRepositoryAssociations_nextToken :: Lens.Lens' DescribeDataRepositoryAssociations (Prelude.Maybe Prelude.Text)
describeDataRepositoryAssociations_nextToken = Lens.lens (\DescribeDataRepositoryAssociations' {nextToken} -> nextToken) (\s@DescribeDataRepositoryAssociations' {} a -> s {nextToken = a} :: DescribeDataRepositoryAssociations)

instance
  Core.AWSRequest
    DescribeDataRepositoryAssociations
  where
  type
    AWSResponse DescribeDataRepositoryAssociations =
      DescribeDataRepositoryAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataRepositoryAssociationsResponse'
            Prelude.<$> (x Data..?> "Associations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDataRepositoryAssociations
  where
  hashWithSalt
    _salt
    DescribeDataRepositoryAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` associationIds
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeDataRepositoryAssociations
  where
  rnf DescribeDataRepositoryAssociations' {..} =
    Prelude.rnf associationIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeDataRepositoryAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DescribeDataRepositoryAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeDataRepositoryAssociations
  where
  toJSON DescribeDataRepositoryAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationIds" Data..=)
              Prelude.<$> associationIds,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeDataRepositoryAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDataRepositoryAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataRepositoryAssociationsResponse' smart constructor.
data DescribeDataRepositoryAssociationsResponse = DescribeDataRepositoryAssociationsResponse'
  { -- | An array of one or more data repository association descriptions.
    associations :: Prelude.Maybe [DataRepositoryAssociation],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataRepositoryAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'describeDataRepositoryAssociationsResponse_associations' - An array of one or more data repository association descriptions.
--
-- 'nextToken', 'describeDataRepositoryAssociationsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeDataRepositoryAssociationsResponse_httpStatus' - The response's http status code.
newDescribeDataRepositoryAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataRepositoryAssociationsResponse
newDescribeDataRepositoryAssociationsResponse
  pHttpStatus_ =
    DescribeDataRepositoryAssociationsResponse'
      { associations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of one or more data repository association descriptions.
describeDataRepositoryAssociationsResponse_associations :: Lens.Lens' DescribeDataRepositoryAssociationsResponse (Prelude.Maybe [DataRepositoryAssociation])
describeDataRepositoryAssociationsResponse_associations = Lens.lens (\DescribeDataRepositoryAssociationsResponse' {associations} -> associations) (\s@DescribeDataRepositoryAssociationsResponse' {} a -> s {associations = a} :: DescribeDataRepositoryAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDataRepositoryAssociationsResponse_nextToken :: Lens.Lens' DescribeDataRepositoryAssociationsResponse (Prelude.Maybe Prelude.Text)
describeDataRepositoryAssociationsResponse_nextToken = Lens.lens (\DescribeDataRepositoryAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeDataRepositoryAssociationsResponse' {} a -> s {nextToken = a} :: DescribeDataRepositoryAssociationsResponse)

-- | The response's http status code.
describeDataRepositoryAssociationsResponse_httpStatus :: Lens.Lens' DescribeDataRepositoryAssociationsResponse Prelude.Int
describeDataRepositoryAssociationsResponse_httpStatus = Lens.lens (\DescribeDataRepositoryAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDataRepositoryAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeDataRepositoryAssociationsResponse)

instance
  Prelude.NFData
    DescribeDataRepositoryAssociationsResponse
  where
  rnf DescribeDataRepositoryAssociationsResponse' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
