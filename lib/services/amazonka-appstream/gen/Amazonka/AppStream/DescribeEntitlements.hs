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
-- Module      : Amazonka.AppStream.DescribeEntitlements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one of more entitlements.
module Amazonka.AppStream.DescribeEntitlements
  ( -- * Creating a Request
    DescribeEntitlements (..),
    newDescribeEntitlements,

    -- * Request Lenses
    describeEntitlements_maxResults,
    describeEntitlements_name,
    describeEntitlements_nextToken,
    describeEntitlements_stackName,

    -- * Destructuring the Response
    DescribeEntitlementsResponse (..),
    newDescribeEntitlementsResponse,

    -- * Response Lenses
    describeEntitlementsResponse_entitlements,
    describeEntitlementsResponse_nextToken,
    describeEntitlementsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEntitlements' smart constructor.
data DescribeEntitlements = DescribeEntitlements'
  { -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the entitlement.
    name :: Prelude.Maybe Prelude.Text,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitlements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeEntitlements_maxResults' - The maximum size of each page of results.
--
-- 'name', 'describeEntitlements_name' - The name of the entitlement.
--
-- 'nextToken', 'describeEntitlements_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'stackName', 'describeEntitlements_stackName' - The name of the stack with which the entitlement is associated.
newDescribeEntitlements ::
  -- | 'stackName'
  Prelude.Text ->
  DescribeEntitlements
newDescribeEntitlements pStackName_ =
  DescribeEntitlements'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stackName = pStackName_
    }

-- | The maximum size of each page of results.
describeEntitlements_maxResults :: Lens.Lens' DescribeEntitlements (Prelude.Maybe Prelude.Int)
describeEntitlements_maxResults = Lens.lens (\DescribeEntitlements' {maxResults} -> maxResults) (\s@DescribeEntitlements' {} a -> s {maxResults = a} :: DescribeEntitlements)

-- | The name of the entitlement.
describeEntitlements_name :: Lens.Lens' DescribeEntitlements (Prelude.Maybe Prelude.Text)
describeEntitlements_name = Lens.lens (\DescribeEntitlements' {name} -> name) (\s@DescribeEntitlements' {} a -> s {name = a} :: DescribeEntitlements)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeEntitlements_nextToken :: Lens.Lens' DescribeEntitlements (Prelude.Maybe Prelude.Text)
describeEntitlements_nextToken = Lens.lens (\DescribeEntitlements' {nextToken} -> nextToken) (\s@DescribeEntitlements' {} a -> s {nextToken = a} :: DescribeEntitlements)

-- | The name of the stack with which the entitlement is associated.
describeEntitlements_stackName :: Lens.Lens' DescribeEntitlements Prelude.Text
describeEntitlements_stackName = Lens.lens (\DescribeEntitlements' {stackName} -> stackName) (\s@DescribeEntitlements' {} a -> s {stackName = a} :: DescribeEntitlements)

instance Core.AWSRequest DescribeEntitlements where
  type
    AWSResponse DescribeEntitlements =
      DescribeEntitlementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntitlementsResponse'
            Prelude.<$> (x Data..?> "Entitlements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEntitlements where
  hashWithSalt _salt DescribeEntitlements' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData DescribeEntitlements where
  rnf DescribeEntitlements' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders DescribeEntitlements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeEntitlements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEntitlements where
  toJSON DescribeEntitlements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("Name" Data..=) Prelude.<$> name,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("StackName" Data..= stackName)
          ]
      )

instance Data.ToPath DescribeEntitlements where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEntitlements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEntitlementsResponse' smart constructor.
data DescribeEntitlementsResponse = DescribeEntitlementsResponse'
  { -- | The entitlements.
    entitlements :: Prelude.Maybe [Entitlement],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitlementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlements', 'describeEntitlementsResponse_entitlements' - The entitlements.
--
-- 'nextToken', 'describeEntitlementsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'describeEntitlementsResponse_httpStatus' - The response's http status code.
newDescribeEntitlementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEntitlementsResponse
newDescribeEntitlementsResponse pHttpStatus_ =
  DescribeEntitlementsResponse'
    { entitlements =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entitlements.
describeEntitlementsResponse_entitlements :: Lens.Lens' DescribeEntitlementsResponse (Prelude.Maybe [Entitlement])
describeEntitlementsResponse_entitlements = Lens.lens (\DescribeEntitlementsResponse' {entitlements} -> entitlements) (\s@DescribeEntitlementsResponse' {} a -> s {entitlements = a} :: DescribeEntitlementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeEntitlementsResponse_nextToken :: Lens.Lens' DescribeEntitlementsResponse (Prelude.Maybe Prelude.Text)
describeEntitlementsResponse_nextToken = Lens.lens (\DescribeEntitlementsResponse' {nextToken} -> nextToken) (\s@DescribeEntitlementsResponse' {} a -> s {nextToken = a} :: DescribeEntitlementsResponse)

-- | The response's http status code.
describeEntitlementsResponse_httpStatus :: Lens.Lens' DescribeEntitlementsResponse Prelude.Int
describeEntitlementsResponse_httpStatus = Lens.lens (\DescribeEntitlementsResponse' {httpStatus} -> httpStatus) (\s@DescribeEntitlementsResponse' {} a -> s {httpStatus = a} :: DescribeEntitlementsResponse)

instance Prelude.NFData DescribeEntitlementsResponse where
  rnf DescribeEntitlementsResponse' {..} =
    Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
