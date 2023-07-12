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
-- Module      : Amazonka.FMS.ListThirdPartyFirewallFirewallPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all of the third-party firewall policies that are
-- associated with the third-party firewall administrator\'s account.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListThirdPartyFirewallFirewallPolicies
  ( -- * Creating a Request
    ListThirdPartyFirewallFirewallPolicies (..),
    newListThirdPartyFirewallFirewallPolicies,

    -- * Request Lenses
    listThirdPartyFirewallFirewallPolicies_nextToken,
    listThirdPartyFirewallFirewallPolicies_thirdPartyFirewall,
    listThirdPartyFirewallFirewallPolicies_maxResults,

    -- * Destructuring the Response
    ListThirdPartyFirewallFirewallPoliciesResponse (..),
    newListThirdPartyFirewallFirewallPoliciesResponse,

    -- * Response Lenses
    listThirdPartyFirewallFirewallPoliciesResponse_nextToken,
    listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies,
    listThirdPartyFirewallFirewallPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThirdPartyFirewallFirewallPolicies' smart constructor.
data ListThirdPartyFirewallFirewallPolicies = ListThirdPartyFirewallFirewallPolicies'
  { -- | If the previous response included a @NextToken@ element, the specified
    -- third-party firewall vendor is associated with more third-party firewall
    -- policies. To get more third-party firewall policies, submit another
    -- @ListThirdPartyFirewallFirewallPoliciesRequest@ request.
    --
    -- For the value of @NextToken@, specify the value of @NextToken@ from the
    -- previous response. If the previous response didn\'t include a
    -- @NextToken@ element, there are no more third-party firewall policies to
    -- get.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the third-party firewall vendor.
    thirdPartyFirewall :: ThirdPartyFirewall,
    -- | The maximum number of third-party firewall policies that you want
    -- Firewall Manager to return. If the specified third-party firewall vendor
    -- is associated with more than @MaxResults@ firewall policies, the
    -- response includes a @NextToken@ element. @NextToken@ contains an
    -- encrypted token that identifies the first third-party firewall policies
    -- that Firewall Manager will return if you submit another request.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThirdPartyFirewallFirewallPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThirdPartyFirewallFirewallPolicies_nextToken' - If the previous response included a @NextToken@ element, the specified
-- third-party firewall vendor is associated with more third-party firewall
-- policies. To get more third-party firewall policies, submit another
-- @ListThirdPartyFirewallFirewallPoliciesRequest@ request.
--
-- For the value of @NextToken@, specify the value of @NextToken@ from the
-- previous response. If the previous response didn\'t include a
-- @NextToken@ element, there are no more third-party firewall policies to
-- get.
--
-- 'thirdPartyFirewall', 'listThirdPartyFirewallFirewallPolicies_thirdPartyFirewall' - The name of the third-party firewall vendor.
--
-- 'maxResults', 'listThirdPartyFirewallFirewallPolicies_maxResults' - The maximum number of third-party firewall policies that you want
-- Firewall Manager to return. If the specified third-party firewall vendor
-- is associated with more than @MaxResults@ firewall policies, the
-- response includes a @NextToken@ element. @NextToken@ contains an
-- encrypted token that identifies the first third-party firewall policies
-- that Firewall Manager will return if you submit another request.
newListThirdPartyFirewallFirewallPolicies ::
  -- | 'thirdPartyFirewall'
  ThirdPartyFirewall ->
  -- | 'maxResults'
  Prelude.Natural ->
  ListThirdPartyFirewallFirewallPolicies
newListThirdPartyFirewallFirewallPolicies
  pThirdPartyFirewall_
  pMaxResults_ =
    ListThirdPartyFirewallFirewallPolicies'
      { nextToken =
          Prelude.Nothing,
        thirdPartyFirewall =
          pThirdPartyFirewall_,
        maxResults = pMaxResults_
      }

-- | If the previous response included a @NextToken@ element, the specified
-- third-party firewall vendor is associated with more third-party firewall
-- policies. To get more third-party firewall policies, submit another
-- @ListThirdPartyFirewallFirewallPoliciesRequest@ request.
--
-- For the value of @NextToken@, specify the value of @NextToken@ from the
-- previous response. If the previous response didn\'t include a
-- @NextToken@ element, there are no more third-party firewall policies to
-- get.
listThirdPartyFirewallFirewallPolicies_nextToken :: Lens.Lens' ListThirdPartyFirewallFirewallPolicies (Prelude.Maybe Prelude.Text)
listThirdPartyFirewallFirewallPolicies_nextToken = Lens.lens (\ListThirdPartyFirewallFirewallPolicies' {nextToken} -> nextToken) (\s@ListThirdPartyFirewallFirewallPolicies' {} a -> s {nextToken = a} :: ListThirdPartyFirewallFirewallPolicies)

-- | The name of the third-party firewall vendor.
listThirdPartyFirewallFirewallPolicies_thirdPartyFirewall :: Lens.Lens' ListThirdPartyFirewallFirewallPolicies ThirdPartyFirewall
listThirdPartyFirewallFirewallPolicies_thirdPartyFirewall = Lens.lens (\ListThirdPartyFirewallFirewallPolicies' {thirdPartyFirewall} -> thirdPartyFirewall) (\s@ListThirdPartyFirewallFirewallPolicies' {} a -> s {thirdPartyFirewall = a} :: ListThirdPartyFirewallFirewallPolicies)

-- | The maximum number of third-party firewall policies that you want
-- Firewall Manager to return. If the specified third-party firewall vendor
-- is associated with more than @MaxResults@ firewall policies, the
-- response includes a @NextToken@ element. @NextToken@ contains an
-- encrypted token that identifies the first third-party firewall policies
-- that Firewall Manager will return if you submit another request.
listThirdPartyFirewallFirewallPolicies_maxResults :: Lens.Lens' ListThirdPartyFirewallFirewallPolicies Prelude.Natural
listThirdPartyFirewallFirewallPolicies_maxResults = Lens.lens (\ListThirdPartyFirewallFirewallPolicies' {maxResults} -> maxResults) (\s@ListThirdPartyFirewallFirewallPolicies' {} a -> s {maxResults = a} :: ListThirdPartyFirewallFirewallPolicies)

instance
  Core.AWSPager
    ListThirdPartyFirewallFirewallPolicies
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThirdPartyFirewallFirewallPoliciesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listThirdPartyFirewallFirewallPolicies_nextToken
          Lens..~ rs
          Lens.^? listThirdPartyFirewallFirewallPoliciesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListThirdPartyFirewallFirewallPolicies
  where
  type
    AWSResponse
      ListThirdPartyFirewallFirewallPolicies =
      ListThirdPartyFirewallFirewallPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThirdPartyFirewallFirewallPoliciesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ThirdPartyFirewallFirewallPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListThirdPartyFirewallFirewallPolicies
  where
  hashWithSalt
    _salt
    ListThirdPartyFirewallFirewallPolicies' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` thirdPartyFirewall
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListThirdPartyFirewallFirewallPolicies
  where
  rnf ListThirdPartyFirewallFirewallPolicies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thirdPartyFirewall
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListThirdPartyFirewallFirewallPolicies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListThirdPartyFirewallFirewallPolicies" ::
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
    ListThirdPartyFirewallFirewallPolicies
  where
  toJSON ListThirdPartyFirewallFirewallPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ThirdPartyFirewall" Data..= thirdPartyFirewall),
            Prelude.Just ("MaxResults" Data..= maxResults)
          ]
      )

instance
  Data.ToPath
    ListThirdPartyFirewallFirewallPolicies
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListThirdPartyFirewallFirewallPolicies
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListThirdPartyFirewallFirewallPoliciesResponse' smart constructor.
data ListThirdPartyFirewallFirewallPoliciesResponse = ListThirdPartyFirewallFirewallPoliciesResponse'
  { -- | The value that you will use for @NextToken@ in the next
    -- @ListThirdPartyFirewallFirewallPolicies@ request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that contains one @ThirdPartyFirewallFirewallPolicies@ element
    -- for each third-party firewall policies that the specified third-party
    -- firewall vendor is associated with. Each
    -- @ThirdPartyFirewallFirewallPolicies@ element contains the firewall
    -- policy name and ID.
    thirdPartyFirewallFirewallPolicies :: Prelude.Maybe [ThirdPartyFirewallFirewallPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThirdPartyFirewallFirewallPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThirdPartyFirewallFirewallPoliciesResponse_nextToken' - The value that you will use for @NextToken@ in the next
-- @ListThirdPartyFirewallFirewallPolicies@ request.
--
-- 'thirdPartyFirewallFirewallPolicies', 'listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies' - A list that contains one @ThirdPartyFirewallFirewallPolicies@ element
-- for each third-party firewall policies that the specified third-party
-- firewall vendor is associated with. Each
-- @ThirdPartyFirewallFirewallPolicies@ element contains the firewall
-- policy name and ID.
--
-- 'httpStatus', 'listThirdPartyFirewallFirewallPoliciesResponse_httpStatus' - The response's http status code.
newListThirdPartyFirewallFirewallPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThirdPartyFirewallFirewallPoliciesResponse
newListThirdPartyFirewallFirewallPoliciesResponse
  pHttpStatus_ =
    ListThirdPartyFirewallFirewallPoliciesResponse'
      { nextToken =
          Prelude.Nothing,
        thirdPartyFirewallFirewallPolicies =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The value that you will use for @NextToken@ in the next
-- @ListThirdPartyFirewallFirewallPolicies@ request.
listThirdPartyFirewallFirewallPoliciesResponse_nextToken :: Lens.Lens' ListThirdPartyFirewallFirewallPoliciesResponse (Prelude.Maybe Prelude.Text)
listThirdPartyFirewallFirewallPoliciesResponse_nextToken = Lens.lens (\ListThirdPartyFirewallFirewallPoliciesResponse' {nextToken} -> nextToken) (\s@ListThirdPartyFirewallFirewallPoliciesResponse' {} a -> s {nextToken = a} :: ListThirdPartyFirewallFirewallPoliciesResponse)

-- | A list that contains one @ThirdPartyFirewallFirewallPolicies@ element
-- for each third-party firewall policies that the specified third-party
-- firewall vendor is associated with. Each
-- @ThirdPartyFirewallFirewallPolicies@ element contains the firewall
-- policy name and ID.
listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies :: Lens.Lens' ListThirdPartyFirewallFirewallPoliciesResponse (Prelude.Maybe [ThirdPartyFirewallFirewallPolicy])
listThirdPartyFirewallFirewallPoliciesResponse_thirdPartyFirewallFirewallPolicies = Lens.lens (\ListThirdPartyFirewallFirewallPoliciesResponse' {thirdPartyFirewallFirewallPolicies} -> thirdPartyFirewallFirewallPolicies) (\s@ListThirdPartyFirewallFirewallPoliciesResponse' {} a -> s {thirdPartyFirewallFirewallPolicies = a} :: ListThirdPartyFirewallFirewallPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThirdPartyFirewallFirewallPoliciesResponse_httpStatus :: Lens.Lens' ListThirdPartyFirewallFirewallPoliciesResponse Prelude.Int
listThirdPartyFirewallFirewallPoliciesResponse_httpStatus = Lens.lens (\ListThirdPartyFirewallFirewallPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListThirdPartyFirewallFirewallPoliciesResponse' {} a -> s {httpStatus = a} :: ListThirdPartyFirewallFirewallPoliciesResponse)

instance
  Prelude.NFData
    ListThirdPartyFirewallFirewallPoliciesResponse
  where
  rnf
    ListThirdPartyFirewallFirewallPoliciesResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf thirdPartyFirewallFirewallPolicies
        `Prelude.seq` Prelude.rnf httpStatus
