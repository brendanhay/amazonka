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
-- Module      : Amazonka.WorkSpaces.DescribeConnectionAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the connection aliases used for
-- cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
module Amazonka.WorkSpaces.DescribeConnectionAliases
  ( -- * Creating a Request
    DescribeConnectionAliases (..),
    newDescribeConnectionAliases,

    -- * Request Lenses
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_limit,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_resourceId,

    -- * Destructuring the Response
    DescribeConnectionAliasesResponse (..),
    newDescribeConnectionAliasesResponse,

    -- * Response Lenses
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeConnectionAliases' smart constructor.
data DescribeConnectionAliases = DescribeConnectionAliases'
  { -- | The identifiers of the connection aliases to describe.
    aliasIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of connection aliases to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory associated with the connection alias.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasIds', 'describeConnectionAliases_aliasIds' - The identifiers of the connection aliases to describe.
--
-- 'limit', 'describeConnectionAliases_limit' - The maximum number of connection aliases to return.
--
-- 'nextToken', 'describeConnectionAliases_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'resourceId', 'describeConnectionAliases_resourceId' - The identifier of the directory associated with the connection alias.
newDescribeConnectionAliases ::
  DescribeConnectionAliases
newDescribeConnectionAliases =
  DescribeConnectionAliases'
    { aliasIds =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The identifiers of the connection aliases to describe.
describeConnectionAliases_aliasIds :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeConnectionAliases_aliasIds = Lens.lens (\DescribeConnectionAliases' {aliasIds} -> aliasIds) (\s@DescribeConnectionAliases' {} a -> s {aliasIds = a} :: DescribeConnectionAliases) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of connection aliases to return.
describeConnectionAliases_limit :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Natural)
describeConnectionAliases_limit = Lens.lens (\DescribeConnectionAliases' {limit} -> limit) (\s@DescribeConnectionAliases' {} a -> s {limit = a} :: DescribeConnectionAliases)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeConnectionAliases_nextToken :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Text)
describeConnectionAliases_nextToken = Lens.lens (\DescribeConnectionAliases' {nextToken} -> nextToken) (\s@DescribeConnectionAliases' {} a -> s {nextToken = a} :: DescribeConnectionAliases)

-- | The identifier of the directory associated with the connection alias.
describeConnectionAliases_resourceId :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Text)
describeConnectionAliases_resourceId = Lens.lens (\DescribeConnectionAliases' {resourceId} -> resourceId) (\s@DescribeConnectionAliases' {} a -> s {resourceId = a} :: DescribeConnectionAliases)

instance Core.AWSRequest DescribeConnectionAliases where
  type
    AWSResponse DescribeConnectionAliases =
      DescribeConnectionAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasesResponse'
            Prelude.<$> (x Data..?> "ConnectionAliases")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnectionAliases where
  hashWithSalt _salt DescribeConnectionAliases' {..} =
    _salt
      `Prelude.hashWithSalt` aliasIds
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DescribeConnectionAliases where
  rnf DescribeConnectionAliases' {..} =
    Prelude.rnf aliasIds
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DescribeConnectionAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeConnectionAliases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnectionAliases where
  toJSON DescribeConnectionAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AliasIds" Data..=) Prelude.<$> aliasIds,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceId" Data..=) Prelude.<$> resourceId
          ]
      )

instance Data.ToPath DescribeConnectionAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConnectionAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectionAliasesResponse' smart constructor.
data DescribeConnectionAliasesResponse = DescribeConnectionAliasesResponse'
  { -- | Information about the specified connection aliases.
    connectionAliases :: Prelude.Maybe (Prelude.NonEmpty ConnectionAlias),
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionAliases', 'describeConnectionAliasesResponse_connectionAliases' - Information about the specified connection aliases.
--
-- 'nextToken', 'describeConnectionAliasesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeConnectionAliasesResponse_httpStatus' - The response's http status code.
newDescribeConnectionAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectionAliasesResponse
newDescribeConnectionAliasesResponse pHttpStatus_ =
  DescribeConnectionAliasesResponse'
    { connectionAliases =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified connection aliases.
describeConnectionAliasesResponse_connectionAliases :: Lens.Lens' DescribeConnectionAliasesResponse (Prelude.Maybe (Prelude.NonEmpty ConnectionAlias))
describeConnectionAliasesResponse_connectionAliases = Lens.lens (\DescribeConnectionAliasesResponse' {connectionAliases} -> connectionAliases) (\s@DescribeConnectionAliasesResponse' {} a -> s {connectionAliases = a} :: DescribeConnectionAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeConnectionAliasesResponse_nextToken :: Lens.Lens' DescribeConnectionAliasesResponse (Prelude.Maybe Prelude.Text)
describeConnectionAliasesResponse_nextToken = Lens.lens (\DescribeConnectionAliasesResponse' {nextToken} -> nextToken) (\s@DescribeConnectionAliasesResponse' {} a -> s {nextToken = a} :: DescribeConnectionAliasesResponse)

-- | The response's http status code.
describeConnectionAliasesResponse_httpStatus :: Lens.Lens' DescribeConnectionAliasesResponse Prelude.Int
describeConnectionAliasesResponse_httpStatus = Lens.lens (\DescribeConnectionAliasesResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionAliasesResponse' {} a -> s {httpStatus = a} :: DescribeConnectionAliasesResponse)

instance
  Prelude.NFData
    DescribeConnectionAliasesResponse
  where
  rnf DescribeConnectionAliasesResponse' {..} =
    Prelude.rnf connectionAliases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
