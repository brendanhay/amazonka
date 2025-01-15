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
-- Module      : Amazonka.IoT.ListThingPrincipals
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals associated with the specified thing. A principal
-- can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito
-- identities or federated identities.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListThingPrincipals>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListThingPrincipals
  ( -- * Creating a Request
    ListThingPrincipals (..),
    newListThingPrincipals,

    -- * Request Lenses
    listThingPrincipals_maxResults,
    listThingPrincipals_nextToken,
    listThingPrincipals_thingName,

    -- * Destructuring the Response
    ListThingPrincipalsResponse (..),
    newListThingPrincipalsResponse,

    -- * Response Lenses
    listThingPrincipalsResponse_nextToken,
    listThingPrincipalsResponse_principals,
    listThingPrincipalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListThingPrincipal operation.
--
-- /See:/ 'newListThingPrincipals' smart constructor.
data ListThingPrincipals = ListThingPrincipals'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingPrincipals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThingPrincipals_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listThingPrincipals_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'thingName', 'listThingPrincipals_thingName' - The name of the thing.
newListThingPrincipals ::
  -- | 'thingName'
  Prelude.Text ->
  ListThingPrincipals
newListThingPrincipals pThingName_ =
  ListThingPrincipals'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The maximum number of results to return in this operation.
listThingPrincipals_maxResults :: Lens.Lens' ListThingPrincipals (Prelude.Maybe Prelude.Natural)
listThingPrincipals_maxResults = Lens.lens (\ListThingPrincipals' {maxResults} -> maxResults) (\s@ListThingPrincipals' {} a -> s {maxResults = a} :: ListThingPrincipals)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingPrincipals_nextToken :: Lens.Lens' ListThingPrincipals (Prelude.Maybe Prelude.Text)
listThingPrincipals_nextToken = Lens.lens (\ListThingPrincipals' {nextToken} -> nextToken) (\s@ListThingPrincipals' {} a -> s {nextToken = a} :: ListThingPrincipals)

-- | The name of the thing.
listThingPrincipals_thingName :: Lens.Lens' ListThingPrincipals Prelude.Text
listThingPrincipals_thingName = Lens.lens (\ListThingPrincipals' {thingName} -> thingName) (\s@ListThingPrincipals' {} a -> s {thingName = a} :: ListThingPrincipals)

instance Core.AWSPager ListThingPrincipals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingPrincipalsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingPrincipalsResponse_principals
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listThingPrincipals_nextToken
              Lens..~ rs
              Lens.^? listThingPrincipalsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListThingPrincipals where
  type
    AWSResponse ListThingPrincipals =
      ListThingPrincipalsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingPrincipalsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "principals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingPrincipals where
  hashWithSalt _salt ListThingPrincipals' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData ListThingPrincipals where
  rnf ListThingPrincipals' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf thingName

instance Data.ToHeaders ListThingPrincipals where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListThingPrincipals where
  toPath ListThingPrincipals' {..} =
    Prelude.mconcat
      ["/things/", Data.toBS thingName, "/principals"]

instance Data.ToQuery ListThingPrincipals where
  toQuery ListThingPrincipals' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | The output from the ListThingPrincipals operation.
--
-- /See:/ 'newListThingPrincipalsResponse' smart constructor.
data ListThingPrincipalsResponse = ListThingPrincipalsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The principals associated with the thing.
    principals :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThingPrincipalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingPrincipalsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'principals', 'listThingPrincipalsResponse_principals' - The principals associated with the thing.
--
-- 'httpStatus', 'listThingPrincipalsResponse_httpStatus' - The response's http status code.
newListThingPrincipalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThingPrincipalsResponse
newListThingPrincipalsResponse pHttpStatus_ =
  ListThingPrincipalsResponse'
    { nextToken =
        Prelude.Nothing,
      principals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingPrincipalsResponse_nextToken :: Lens.Lens' ListThingPrincipalsResponse (Prelude.Maybe Prelude.Text)
listThingPrincipalsResponse_nextToken = Lens.lens (\ListThingPrincipalsResponse' {nextToken} -> nextToken) (\s@ListThingPrincipalsResponse' {} a -> s {nextToken = a} :: ListThingPrincipalsResponse)

-- | The principals associated with the thing.
listThingPrincipalsResponse_principals :: Lens.Lens' ListThingPrincipalsResponse (Prelude.Maybe [Prelude.Text])
listThingPrincipalsResponse_principals = Lens.lens (\ListThingPrincipalsResponse' {principals} -> principals) (\s@ListThingPrincipalsResponse' {} a -> s {principals = a} :: ListThingPrincipalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listThingPrincipalsResponse_httpStatus :: Lens.Lens' ListThingPrincipalsResponse Prelude.Int
listThingPrincipalsResponse_httpStatus = Lens.lens (\ListThingPrincipalsResponse' {httpStatus} -> httpStatus) (\s@ListThingPrincipalsResponse' {} a -> s {httpStatus = a} :: ListThingPrincipalsResponse)

instance Prelude.NFData ListThingPrincipalsResponse where
  rnf ListThingPrincipalsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf principals `Prelude.seq`
        Prelude.rnf httpStatus
