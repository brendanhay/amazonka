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
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the connection aliases used for
-- cross-Region redirection. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
module Network.AWS.WorkSpaces.DescribeConnectionAliases
  ( -- * Creating a Request
    DescribeConnectionAliases (..),
    newDescribeConnectionAliases,

    -- * Request Lenses
    describeConnectionAliases_resourceId,
    describeConnectionAliases_nextToken,
    describeConnectionAliases_aliasIds,
    describeConnectionAliases_limit,

    -- * Destructuring the Response
    DescribeConnectionAliasesResponse (..),
    newDescribeConnectionAliasesResponse,

    -- * Response Lenses
    describeConnectionAliasesResponse_nextToken,
    describeConnectionAliasesResponse_connectionAliases,
    describeConnectionAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeConnectionAliases' smart constructor.
data DescribeConnectionAliases = DescribeConnectionAliases'
  { -- | The identifier of the directory associated with the connection alias.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the connection aliases to describe.
    aliasIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of connection aliases to return.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeConnectionAliases_resourceId' - The identifier of the directory associated with the connection alias.
--
-- 'nextToken', 'describeConnectionAliases_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'aliasIds', 'describeConnectionAliases_aliasIds' - The identifiers of the connection aliases to describe.
--
-- 'limit', 'describeConnectionAliases_limit' - The maximum number of connection aliases to return.
newDescribeConnectionAliases ::
  DescribeConnectionAliases
newDescribeConnectionAliases =
  DescribeConnectionAliases'
    { resourceId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      aliasIds = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The identifier of the directory associated with the connection alias.
describeConnectionAliases_resourceId :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Text)
describeConnectionAliases_resourceId = Lens.lens (\DescribeConnectionAliases' {resourceId} -> resourceId) (\s@DescribeConnectionAliases' {} a -> s {resourceId = a} :: DescribeConnectionAliases)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeConnectionAliases_nextToken :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Text)
describeConnectionAliases_nextToken = Lens.lens (\DescribeConnectionAliases' {nextToken} -> nextToken) (\s@DescribeConnectionAliases' {} a -> s {nextToken = a} :: DescribeConnectionAliases)

-- | The identifiers of the connection aliases to describe.
describeConnectionAliases_aliasIds :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeConnectionAliases_aliasIds = Lens.lens (\DescribeConnectionAliases' {aliasIds} -> aliasIds) (\s@DescribeConnectionAliases' {} a -> s {aliasIds = a} :: DescribeConnectionAliases) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of connection aliases to return.
describeConnectionAliases_limit :: Lens.Lens' DescribeConnectionAliases (Prelude.Maybe Prelude.Natural)
describeConnectionAliases_limit = Lens.lens (\DescribeConnectionAliases' {limit} -> limit) (\s@DescribeConnectionAliases' {} a -> s {limit = a} :: DescribeConnectionAliases)

instance Prelude.AWSRequest DescribeConnectionAliases where
  type
    Rs DescribeConnectionAliases =
      DescribeConnectionAliasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "ConnectionAliases")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnectionAliases

instance Prelude.NFData DescribeConnectionAliases

instance Prelude.ToHeaders DescribeConnectionAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DescribeConnectionAliases" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeConnectionAliases where
  toJSON DescribeConnectionAliases' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceId" Prelude..=) Prelude.<$> resourceId,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("AliasIds" Prelude..=) Prelude.<$> aliasIds,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath DescribeConnectionAliases where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeConnectionAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectionAliasesResponse' smart constructor.
data DescribeConnectionAliasesResponse = DescribeConnectionAliasesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the specified connection aliases.
    connectionAliases :: Prelude.Maybe (Prelude.NonEmpty ConnectionAlias),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConnectionAliasesResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'connectionAliases', 'describeConnectionAliasesResponse_connectionAliases' - Information about the specified connection aliases.
--
-- 'httpStatus', 'describeConnectionAliasesResponse_httpStatus' - The response's http status code.
newDescribeConnectionAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectionAliasesResponse
newDescribeConnectionAliasesResponse pHttpStatus_ =
  DescribeConnectionAliasesResponse'
    { nextToken =
        Prelude.Nothing,
      connectionAliases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeConnectionAliasesResponse_nextToken :: Lens.Lens' DescribeConnectionAliasesResponse (Prelude.Maybe Prelude.Text)
describeConnectionAliasesResponse_nextToken = Lens.lens (\DescribeConnectionAliasesResponse' {nextToken} -> nextToken) (\s@DescribeConnectionAliasesResponse' {} a -> s {nextToken = a} :: DescribeConnectionAliasesResponse)

-- | Information about the specified connection aliases.
describeConnectionAliasesResponse_connectionAliases :: Lens.Lens' DescribeConnectionAliasesResponse (Prelude.Maybe (Prelude.NonEmpty ConnectionAlias))
describeConnectionAliasesResponse_connectionAliases = Lens.lens (\DescribeConnectionAliasesResponse' {connectionAliases} -> connectionAliases) (\s@DescribeConnectionAliasesResponse' {} a -> s {connectionAliases = a} :: DescribeConnectionAliasesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeConnectionAliasesResponse_httpStatus :: Lens.Lens' DescribeConnectionAliasesResponse Prelude.Int
describeConnectionAliasesResponse_httpStatus = Lens.lens (\DescribeConnectionAliasesResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionAliasesResponse' {} a -> s {httpStatus = a} :: DescribeConnectionAliasesResponse)

instance
  Prelude.NFData
    DescribeConnectionAliasesResponse
