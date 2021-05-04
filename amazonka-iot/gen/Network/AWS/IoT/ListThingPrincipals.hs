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
-- Module      : Network.AWS.IoT.ListThingPrincipals
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals associated with the specified thing. A principal
-- can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito
-- identities or federated identities.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingPrincipals
  ( -- * Creating a Request
    ListThingPrincipals (..),
    newListThingPrincipals,

    -- * Request Lenses
    listThingPrincipals_nextToken,
    listThingPrincipals_maxResults,
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThingPrincipal operation.
--
-- /See:/ 'newListThingPrincipals' smart constructor.
data ListThingPrincipals = ListThingPrincipals'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the thing.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListThingPrincipals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingPrincipals_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listThingPrincipals_maxResults' - The maximum number of results to return in this operation.
--
-- 'thingName', 'listThingPrincipals_thingName' - The name of the thing.
newListThingPrincipals ::
  -- | 'thingName'
  Prelude.Text ->
  ListThingPrincipals
newListThingPrincipals pThingName_ =
  ListThingPrincipals'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      thingName = pThingName_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingPrincipals_nextToken :: Lens.Lens' ListThingPrincipals (Prelude.Maybe Prelude.Text)
listThingPrincipals_nextToken = Lens.lens (\ListThingPrincipals' {nextToken} -> nextToken) (\s@ListThingPrincipals' {} a -> s {nextToken = a} :: ListThingPrincipals)

-- | The maximum number of results to return in this operation.
listThingPrincipals_maxResults :: Lens.Lens' ListThingPrincipals (Prelude.Maybe Prelude.Natural)
listThingPrincipals_maxResults = Lens.lens (\ListThingPrincipals' {maxResults} -> maxResults) (\s@ListThingPrincipals' {} a -> s {maxResults = a} :: ListThingPrincipals)

-- | The name of the thing.
listThingPrincipals_thingName :: Lens.Lens' ListThingPrincipals Prelude.Text
listThingPrincipals_thingName = Lens.lens (\ListThingPrincipals' {thingName} -> thingName) (\s@ListThingPrincipals' {} a -> s {thingName = a} :: ListThingPrincipals)

instance Pager.AWSPager ListThingPrincipals where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listThingPrincipalsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listThingPrincipalsResponse_principals
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listThingPrincipals_nextToken
          Lens..~ rs
          Lens.^? listThingPrincipalsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListThingPrincipals where
  type
    Rs ListThingPrincipals =
      ListThingPrincipalsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingPrincipalsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "principals"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingPrincipals

instance Prelude.NFData ListThingPrincipals

instance Prelude.ToHeaders ListThingPrincipals where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListThingPrincipals where
  toPath ListThingPrincipals' {..} =
    Prelude.mconcat
      ["/things/", Prelude.toBS thingName, "/principals"]

instance Prelude.ToQuery ListThingPrincipals where
  toQuery ListThingPrincipals' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listThingPrincipalsResponse_principals = Lens.lens (\ListThingPrincipalsResponse' {principals} -> principals) (\s@ListThingPrincipalsResponse' {} a -> s {principals = a} :: ListThingPrincipalsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listThingPrincipalsResponse_httpStatus :: Lens.Lens' ListThingPrincipalsResponse Prelude.Int
listThingPrincipalsResponse_httpStatus = Lens.lens (\ListThingPrincipalsResponse' {httpStatus} -> httpStatus) (\s@ListThingPrincipalsResponse' {} a -> s {httpStatus = a} :: ListThingPrincipalsResponse)

instance Prelude.NFData ListThingPrincipalsResponse
