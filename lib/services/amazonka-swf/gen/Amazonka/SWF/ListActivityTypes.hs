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
-- Module      : Amazonka.SWF.ListActivityTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all activities registered in the specified
-- domain that match the specified name and registration status. The result
-- includes information like creation date, current status of the activity,
-- etc. The results may be split into multiple pages. To retrieve
-- subsequent pages, make the call again using the @nextPageToken@ returned
-- by the initial call.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SWF.ListActivityTypes
  ( -- * Creating a Request
    ListActivityTypes (..),
    newListActivityTypes,

    -- * Request Lenses
    listActivityTypes_maximumPageSize,
    listActivityTypes_name,
    listActivityTypes_nextPageToken,
    listActivityTypes_reverseOrder,
    listActivityTypes_domain,
    listActivityTypes_registrationStatus,

    -- * Destructuring the Response
    ListActivityTypesResponse (..),
    newListActivityTypesResponse,

    -- * Response Lenses
    listActivityTypesResponse_nextPageToken,
    listActivityTypesResponse_httpStatus,
    listActivityTypesResponse_typeInfos,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newListActivityTypes' smart constructor.
data ListActivityTypes = ListActivityTypes'
  { -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | If specified, only lists the activity types that have this name.
    name :: Prelude.Maybe Prelude.Text,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 24 hours. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, returns the results in reverse order. By default,
    -- the results are returned in ascending alphabetical order by @name@ of
    -- the activity types.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain in which the activity types have been registered.
    domain :: Prelude.Text,
    -- | Specifies the registration status of the activity types to list.
    registrationStatus :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActivityTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumPageSize', 'listActivityTypes_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'name', 'listActivityTypes_name' - If specified, only lists the activity types that have this name.
--
-- 'nextPageToken', 'listActivityTypes_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 24 hours. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'reverseOrder', 'listActivityTypes_reverseOrder' - When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the activity types.
--
-- 'domain', 'listActivityTypes_domain' - The name of the domain in which the activity types have been registered.
--
-- 'registrationStatus', 'listActivityTypes_registrationStatus' - Specifies the registration status of the activity types to list.
newListActivityTypes ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListActivityTypes
newListActivityTypes pDomain_ pRegistrationStatus_ =
  ListActivityTypes'
    { maximumPageSize =
        Prelude.Nothing,
      name = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      domain = pDomain_,
      registrationStatus = pRegistrationStatus_
    }

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listActivityTypes_maximumPageSize :: Lens.Lens' ListActivityTypes (Prelude.Maybe Prelude.Natural)
listActivityTypes_maximumPageSize = Lens.lens (\ListActivityTypes' {maximumPageSize} -> maximumPageSize) (\s@ListActivityTypes' {} a -> s {maximumPageSize = a} :: ListActivityTypes)

-- | If specified, only lists the activity types that have this name.
listActivityTypes_name :: Lens.Lens' ListActivityTypes (Prelude.Maybe Prelude.Text)
listActivityTypes_name = Lens.lens (\ListActivityTypes' {name} -> name) (\s@ListActivityTypes' {} a -> s {name = a} :: ListActivityTypes)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 24 hours. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listActivityTypes_nextPageToken :: Lens.Lens' ListActivityTypes (Prelude.Maybe Prelude.Text)
listActivityTypes_nextPageToken = Lens.lens (\ListActivityTypes' {nextPageToken} -> nextPageToken) (\s@ListActivityTypes' {} a -> s {nextPageToken = a} :: ListActivityTypes)

-- | When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the activity types.
listActivityTypes_reverseOrder :: Lens.Lens' ListActivityTypes (Prelude.Maybe Prelude.Bool)
listActivityTypes_reverseOrder = Lens.lens (\ListActivityTypes' {reverseOrder} -> reverseOrder) (\s@ListActivityTypes' {} a -> s {reverseOrder = a} :: ListActivityTypes)

-- | The name of the domain in which the activity types have been registered.
listActivityTypes_domain :: Lens.Lens' ListActivityTypes Prelude.Text
listActivityTypes_domain = Lens.lens (\ListActivityTypes' {domain} -> domain) (\s@ListActivityTypes' {} a -> s {domain = a} :: ListActivityTypes)

-- | Specifies the registration status of the activity types to list.
listActivityTypes_registrationStatus :: Lens.Lens' ListActivityTypes RegistrationStatus
listActivityTypes_registrationStatus = Lens.lens (\ListActivityTypes' {registrationStatus} -> registrationStatus) (\s@ListActivityTypes' {} a -> s {registrationStatus = a} :: ListActivityTypes)

instance Core.AWSPager ListActivityTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActivityTypesResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listActivityTypesResponse_typeInfos) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listActivityTypes_nextPageToken
          Lens..~ rs
          Lens.^? listActivityTypesResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListActivityTypes where
  type
    AWSResponse ListActivityTypes =
      ListActivityTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActivityTypesResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "typeInfos" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListActivityTypes where
  hashWithSalt _salt ListActivityTypes' {..} =
    _salt
      `Prelude.hashWithSalt` maximumPageSize
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` registrationStatus

instance Prelude.NFData ListActivityTypes where
  rnf ListActivityTypes' {..} =
    Prelude.rnf maximumPageSize
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf registrationStatus

instance Data.ToHeaders ListActivityTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.ListActivityTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListActivityTypes where
  toJSON ListActivityTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maximumPageSize" Data..=)
              Prelude.<$> maximumPageSize,
            ("name" Data..=) Prelude.<$> name,
            ("nextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("reverseOrder" Data..=) Prelude.<$> reverseOrder,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just
              ("registrationStatus" Data..= registrationStatus)
          ]
      )

instance Data.ToPath ListActivityTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListActivityTypes where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a paginated list of activity type information structures.
--
-- /See:/ 'newListActivityTypesResponse' smart constructor.
data ListActivityTypesResponse = ListActivityTypesResponse'
  { -- | If a @NextPageToken@ was returned by a previous call, there are more
    -- results available. To retrieve the next page of results, make the call
    -- again using the returned token in @nextPageToken@. Keep all other
    -- arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of activity type information.
    typeInfos :: [ActivityTypeInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActivityTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listActivityTypesResponse_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'httpStatus', 'listActivityTypesResponse_httpStatus' - The response's http status code.
--
-- 'typeInfos', 'listActivityTypesResponse_typeInfos' - List of activity type information.
newListActivityTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActivityTypesResponse
newListActivityTypesResponse pHttpStatus_ =
  ListActivityTypesResponse'
    { nextPageToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      typeInfos = Prelude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listActivityTypesResponse_nextPageToken :: Lens.Lens' ListActivityTypesResponse (Prelude.Maybe Prelude.Text)
listActivityTypesResponse_nextPageToken = Lens.lens (\ListActivityTypesResponse' {nextPageToken} -> nextPageToken) (\s@ListActivityTypesResponse' {} a -> s {nextPageToken = a} :: ListActivityTypesResponse)

-- | The response's http status code.
listActivityTypesResponse_httpStatus :: Lens.Lens' ListActivityTypesResponse Prelude.Int
listActivityTypesResponse_httpStatus = Lens.lens (\ListActivityTypesResponse' {httpStatus} -> httpStatus) (\s@ListActivityTypesResponse' {} a -> s {httpStatus = a} :: ListActivityTypesResponse)

-- | List of activity type information.
listActivityTypesResponse_typeInfos :: Lens.Lens' ListActivityTypesResponse [ActivityTypeInfo]
listActivityTypesResponse_typeInfos = Lens.lens (\ListActivityTypesResponse' {typeInfos} -> typeInfos) (\s@ListActivityTypesResponse' {} a -> s {typeInfos = a} :: ListActivityTypesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListActivityTypesResponse where
  rnf ListActivityTypesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf typeInfos
