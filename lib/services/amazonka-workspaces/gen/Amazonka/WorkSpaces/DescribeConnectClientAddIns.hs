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
-- Module      : Amazonka.WorkSpaces.DescribeConnectClientAddIns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Amazon Connect client add-ins that have been
-- created.
module Amazonka.WorkSpaces.DescribeConnectClientAddIns
  ( -- * Creating a Request
    DescribeConnectClientAddIns (..),
    newDescribeConnectClientAddIns,

    -- * Request Lenses
    describeConnectClientAddIns_nextToken,
    describeConnectClientAddIns_maxResults,
    describeConnectClientAddIns_resourceId,

    -- * Destructuring the Response
    DescribeConnectClientAddInsResponse (..),
    newDescribeConnectClientAddInsResponse,

    -- * Response Lenses
    describeConnectClientAddInsResponse_nextToken,
    describeConnectClientAddInsResponse_addIns,
    describeConnectClientAddInsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeConnectClientAddIns' smart constructor.
data DescribeConnectClientAddIns = DescribeConnectClientAddIns'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The directory identifier for which the client add-in is configured.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectClientAddIns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConnectClientAddIns_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'maxResults', 'describeConnectClientAddIns_maxResults' - The maximum number of items to return.
--
-- 'resourceId', 'describeConnectClientAddIns_resourceId' - The directory identifier for which the client add-in is configured.
newDescribeConnectClientAddIns ::
  -- | 'resourceId'
  Prelude.Text ->
  DescribeConnectClientAddIns
newDescribeConnectClientAddIns pResourceId_ =
  DescribeConnectClientAddIns'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeConnectClientAddIns_nextToken :: Lens.Lens' DescribeConnectClientAddIns (Prelude.Maybe Prelude.Text)
describeConnectClientAddIns_nextToken = Lens.lens (\DescribeConnectClientAddIns' {nextToken} -> nextToken) (\s@DescribeConnectClientAddIns' {} a -> s {nextToken = a} :: DescribeConnectClientAddIns)

-- | The maximum number of items to return.
describeConnectClientAddIns_maxResults :: Lens.Lens' DescribeConnectClientAddIns (Prelude.Maybe Prelude.Natural)
describeConnectClientAddIns_maxResults = Lens.lens (\DescribeConnectClientAddIns' {maxResults} -> maxResults) (\s@DescribeConnectClientAddIns' {} a -> s {maxResults = a} :: DescribeConnectClientAddIns)

-- | The directory identifier for which the client add-in is configured.
describeConnectClientAddIns_resourceId :: Lens.Lens' DescribeConnectClientAddIns Prelude.Text
describeConnectClientAddIns_resourceId = Lens.lens (\DescribeConnectClientAddIns' {resourceId} -> resourceId) (\s@DescribeConnectClientAddIns' {} a -> s {resourceId = a} :: DescribeConnectClientAddIns)

instance Core.AWSRequest DescribeConnectClientAddIns where
  type
    AWSResponse DescribeConnectClientAddIns =
      DescribeConnectClientAddInsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectClientAddInsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "AddIns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnectClientAddIns where
  hashWithSalt _salt DescribeConnectClientAddIns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DescribeConnectClientAddIns where
  rnf DescribeConnectClientAddIns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DescribeConnectClientAddIns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeConnectClientAddIns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnectClientAddIns where
  toJSON DescribeConnectClientAddIns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DescribeConnectClientAddIns where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConnectClientAddIns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectClientAddInsResponse' smart constructor.
data DescribeConnectClientAddInsResponse = DescribeConnectClientAddInsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about client add-ins.
    addIns :: Prelude.Maybe [ConnectClientAddIn],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectClientAddInsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConnectClientAddInsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'addIns', 'describeConnectClientAddInsResponse_addIns' - Information about client add-ins.
--
-- 'httpStatus', 'describeConnectClientAddInsResponse_httpStatus' - The response's http status code.
newDescribeConnectClientAddInsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectClientAddInsResponse
newDescribeConnectClientAddInsResponse pHttpStatus_ =
  DescribeConnectClientAddInsResponse'
    { nextToken =
        Prelude.Nothing,
      addIns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeConnectClientAddInsResponse_nextToken :: Lens.Lens' DescribeConnectClientAddInsResponse (Prelude.Maybe Prelude.Text)
describeConnectClientAddInsResponse_nextToken = Lens.lens (\DescribeConnectClientAddInsResponse' {nextToken} -> nextToken) (\s@DescribeConnectClientAddInsResponse' {} a -> s {nextToken = a} :: DescribeConnectClientAddInsResponse)

-- | Information about client add-ins.
describeConnectClientAddInsResponse_addIns :: Lens.Lens' DescribeConnectClientAddInsResponse (Prelude.Maybe [ConnectClientAddIn])
describeConnectClientAddInsResponse_addIns = Lens.lens (\DescribeConnectClientAddInsResponse' {addIns} -> addIns) (\s@DescribeConnectClientAddInsResponse' {} a -> s {addIns = a} :: DescribeConnectClientAddInsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConnectClientAddInsResponse_httpStatus :: Lens.Lens' DescribeConnectClientAddInsResponse Prelude.Int
describeConnectClientAddInsResponse_httpStatus = Lens.lens (\DescribeConnectClientAddInsResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectClientAddInsResponse' {} a -> s {httpStatus = a} :: DescribeConnectClientAddInsResponse)

instance
  Prelude.NFData
    DescribeConnectClientAddInsResponse
  where
  rnf DescribeConnectClientAddInsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf addIns
      `Prelude.seq` Prelude.rnf httpStatus
