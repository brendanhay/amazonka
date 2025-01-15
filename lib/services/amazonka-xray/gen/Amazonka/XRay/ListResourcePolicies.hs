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
-- Module      : Amazonka.XRay.ListResourcePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of resource policies in the target Amazon Web Services
-- account.
--
-- This operation returns paginated results.
module Amazonka.XRay.ListResourcePolicies
  ( -- * Creating a Request
    ListResourcePolicies (..),
    newListResourcePolicies,

    -- * Request Lenses
    listResourcePolicies_nextToken,

    -- * Destructuring the Response
    ListResourcePoliciesResponse (..),
    newListResourcePoliciesResponse,

    -- * Response Lenses
    listResourcePoliciesResponse_nextToken,
    listResourcePoliciesResponse_resourcePolicies,
    listResourcePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newListResourcePolicies' smart constructor.
data ListResourcePolicies = ListResourcePolicies'
  { -- | Not currently supported.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcePolicies_nextToken' - Not currently supported.
newListResourcePolicies ::
  ListResourcePolicies
newListResourcePolicies =
  ListResourcePolicies' {nextToken = Prelude.Nothing}

-- | Not currently supported.
listResourcePolicies_nextToken :: Lens.Lens' ListResourcePolicies (Prelude.Maybe Prelude.Text)
listResourcePolicies_nextToken = Lens.lens (\ListResourcePolicies' {nextToken} -> nextToken) (\s@ListResourcePolicies' {} a -> s {nextToken = a} :: ListResourcePolicies)

instance Core.AWSPager ListResourcePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcePoliciesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcePoliciesResponse_resourcePolicies
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listResourcePolicies_nextToken
              Lens..~ rs
              Lens.^? listResourcePoliciesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListResourcePolicies where
  type
    AWSResponse ListResourcePolicies =
      ListResourcePoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcePoliciesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ResourcePolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourcePolicies where
  hashWithSalt _salt ListResourcePolicies' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResourcePolicies where
  rnf ListResourcePolicies' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListResourcePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListResourcePolicies where
  toJSON ListResourcePolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListResourcePolicies where
  toPath = Prelude.const "/ListResourcePolicies"

instance Data.ToQuery ListResourcePolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcePoliciesResponse' smart constructor.
data ListResourcePoliciesResponse = ListResourcePoliciesResponse'
  { -- | Pagination token. Not currently supported.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of resource policies in the target Amazon Web Services account.
    resourcePolicies :: Prelude.Maybe [ResourcePolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcePoliciesResponse_nextToken' - Pagination token. Not currently supported.
--
-- 'resourcePolicies', 'listResourcePoliciesResponse_resourcePolicies' - The list of resource policies in the target Amazon Web Services account.
--
-- 'httpStatus', 'listResourcePoliciesResponse_httpStatus' - The response's http status code.
newListResourcePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcePoliciesResponse
newListResourcePoliciesResponse pHttpStatus_ =
  ListResourcePoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      resourcePolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token. Not currently supported.
listResourcePoliciesResponse_nextToken :: Lens.Lens' ListResourcePoliciesResponse (Prelude.Maybe Prelude.Text)
listResourcePoliciesResponse_nextToken = Lens.lens (\ListResourcePoliciesResponse' {nextToken} -> nextToken) (\s@ListResourcePoliciesResponse' {} a -> s {nextToken = a} :: ListResourcePoliciesResponse)

-- | The list of resource policies in the target Amazon Web Services account.
listResourcePoliciesResponse_resourcePolicies :: Lens.Lens' ListResourcePoliciesResponse (Prelude.Maybe [ResourcePolicy])
listResourcePoliciesResponse_resourcePolicies = Lens.lens (\ListResourcePoliciesResponse' {resourcePolicies} -> resourcePolicies) (\s@ListResourcePoliciesResponse' {} a -> s {resourcePolicies = a} :: ListResourcePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourcePoliciesResponse_httpStatus :: Lens.Lens' ListResourcePoliciesResponse Prelude.Int
listResourcePoliciesResponse_httpStatus = Lens.lens (\ListResourcePoliciesResponse' {httpStatus} -> httpStatus) (\s@ListResourcePoliciesResponse' {} a -> s {httpStatus = a} :: ListResourcePoliciesResponse)

instance Prelude.NFData ListResourcePoliciesResponse where
  rnf ListResourcePoliciesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf resourcePolicies `Prelude.seq`
        Prelude.rnf httpStatus
