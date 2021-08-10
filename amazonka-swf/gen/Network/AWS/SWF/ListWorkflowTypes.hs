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
-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about workflow types in the specified domain. The
-- results may be split into multiple pages that can be retrieved by making
-- the call repeatedly.
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
module Network.AWS.SWF.ListWorkflowTypes
  ( -- * Creating a Request
    ListWorkflowTypes (..),
    newListWorkflowTypes,

    -- * Request Lenses
    listWorkflowTypes_name,
    listWorkflowTypes_nextPageToken,
    listWorkflowTypes_maximumPageSize,
    listWorkflowTypes_reverseOrder,
    listWorkflowTypes_domain,
    listWorkflowTypes_registrationStatus,

    -- * Destructuring the Response
    ListWorkflowTypesResponse (..),
    newListWorkflowTypesResponse,

    -- * Response Lenses
    listWorkflowTypesResponse_nextPageToken,
    listWorkflowTypesResponse_httpStatus,
    listWorkflowTypesResponse_typeInfos,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newListWorkflowTypes' smart constructor.
data ListWorkflowTypes = ListWorkflowTypes'
  { -- | If specified, lists the workflow type with this name.
    name :: Prelude.Maybe Prelude.Text,
    -- | If @NextPageToken@ is returned there are more results available. The
    -- value of @NextPageToken@ is a unique pagination token for each page.
    -- Make the call again using the returned token to retrieve the next page.
    -- Keep all other arguments unchanged. Each pagination token expires after
    -- 60 seconds. Using an expired pagination token will return a @400@ error:
    -- \"@Specified token has exceeded its maximum lifetime@\".
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. Use
    -- @nextPageToken@ to obtain further pages of results.
    maximumPageSize :: Prelude.Maybe Prelude.Natural,
    -- | When set to @true@, returns the results in reverse order. By default the
    -- results are returned in ascending alphabetical order of the @name@ of
    -- the workflow types.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain in which the workflow types have been registered.
    domain :: Prelude.Text,
    -- | Specifies the registration status of the workflow types to list.
    registrationStatus :: RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listWorkflowTypes_name' - If specified, lists the workflow type with this name.
--
-- 'nextPageToken', 'listWorkflowTypes_nextPageToken' - If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'maximumPageSize', 'listWorkflowTypes_maximumPageSize' - The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
--
-- 'reverseOrder', 'listWorkflowTypes_reverseOrder' - When set to @true@, returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the @name@ of
-- the workflow types.
--
-- 'domain', 'listWorkflowTypes_domain' - The name of the domain in which the workflow types have been registered.
--
-- 'registrationStatus', 'listWorkflowTypes_registrationStatus' - Specifies the registration status of the workflow types to list.
newListWorkflowTypes ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListWorkflowTypes
newListWorkflowTypes pDomain_ pRegistrationStatus_ =
  ListWorkflowTypes'
    { name = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      maximumPageSize = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      domain = pDomain_,
      registrationStatus = pRegistrationStatus_
    }

-- | If specified, lists the workflow type with this name.
listWorkflowTypes_name :: Lens.Lens' ListWorkflowTypes (Prelude.Maybe Prelude.Text)
listWorkflowTypes_name = Lens.lens (\ListWorkflowTypes' {name} -> name) (\s@ListWorkflowTypes' {} a -> s {name = a} :: ListWorkflowTypes)

-- | If @NextPageToken@ is returned there are more results available. The
-- value of @NextPageToken@ is a unique pagination token for each page.
-- Make the call again using the returned token to retrieve the next page.
-- Keep all other arguments unchanged. Each pagination token expires after
-- 60 seconds. Using an expired pagination token will return a @400@ error:
-- \"@Specified token has exceeded its maximum lifetime@\".
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
listWorkflowTypes_nextPageToken :: Lens.Lens' ListWorkflowTypes (Prelude.Maybe Prelude.Text)
listWorkflowTypes_nextPageToken = Lens.lens (\ListWorkflowTypes' {nextPageToken} -> nextPageToken) (\s@ListWorkflowTypes' {} a -> s {nextPageToken = a} :: ListWorkflowTypes)

-- | The maximum number of results that are returned per call. Use
-- @nextPageToken@ to obtain further pages of results.
listWorkflowTypes_maximumPageSize :: Lens.Lens' ListWorkflowTypes (Prelude.Maybe Prelude.Natural)
listWorkflowTypes_maximumPageSize = Lens.lens (\ListWorkflowTypes' {maximumPageSize} -> maximumPageSize) (\s@ListWorkflowTypes' {} a -> s {maximumPageSize = a} :: ListWorkflowTypes)

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the @name@ of
-- the workflow types.
listWorkflowTypes_reverseOrder :: Lens.Lens' ListWorkflowTypes (Prelude.Maybe Prelude.Bool)
listWorkflowTypes_reverseOrder = Lens.lens (\ListWorkflowTypes' {reverseOrder} -> reverseOrder) (\s@ListWorkflowTypes' {} a -> s {reverseOrder = a} :: ListWorkflowTypes)

-- | The name of the domain in which the workflow types have been registered.
listWorkflowTypes_domain :: Lens.Lens' ListWorkflowTypes Prelude.Text
listWorkflowTypes_domain = Lens.lens (\ListWorkflowTypes' {domain} -> domain) (\s@ListWorkflowTypes' {} a -> s {domain = a} :: ListWorkflowTypes)

-- | Specifies the registration status of the workflow types to list.
listWorkflowTypes_registrationStatus :: Lens.Lens' ListWorkflowTypes RegistrationStatus
listWorkflowTypes_registrationStatus = Lens.lens (\ListWorkflowTypes' {registrationStatus} -> registrationStatus) (\s@ListWorkflowTypes' {} a -> s {registrationStatus = a} :: ListWorkflowTypes)

instance Core.AWSPager ListWorkflowTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkflowTypesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listWorkflowTypesResponse_typeInfos) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkflowTypes_nextPageToken
          Lens..~ rs
          Lens.^? listWorkflowTypesResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorkflowTypes where
  type
    AWSResponse ListWorkflowTypes =
      ListWorkflowTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowTypesResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "typeInfos" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListWorkflowTypes

instance Prelude.NFData ListWorkflowTypes

instance Core.ToHeaders ListWorkflowTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.ListWorkflowTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWorkflowTypes where
  toJSON ListWorkflowTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("nextPageToken" Core..=) Prelude.<$> nextPageToken,
            ("maximumPageSize" Core..=)
              Prelude.<$> maximumPageSize,
            ("reverseOrder" Core..=) Prelude.<$> reverseOrder,
            Prelude.Just ("domain" Core..= domain),
            Prelude.Just
              ("registrationStatus" Core..= registrationStatus)
          ]
      )

instance Core.ToPath ListWorkflowTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListWorkflowTypes where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a paginated list of information structures about workflow
-- types.
--
-- /See:/ 'newListWorkflowTypesResponse' smart constructor.
data ListWorkflowTypesResponse = ListWorkflowTypesResponse'
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
    -- | The list of workflow type information.
    typeInfos :: [WorkflowTypeInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listWorkflowTypesResponse_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'httpStatus', 'listWorkflowTypesResponse_httpStatus' - The response's http status code.
--
-- 'typeInfos', 'listWorkflowTypesResponse_typeInfos' - The list of workflow type information.
newListWorkflowTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowTypesResponse
newListWorkflowTypesResponse pHttpStatus_ =
  ListWorkflowTypesResponse'
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
listWorkflowTypesResponse_nextPageToken :: Lens.Lens' ListWorkflowTypesResponse (Prelude.Maybe Prelude.Text)
listWorkflowTypesResponse_nextPageToken = Lens.lens (\ListWorkflowTypesResponse' {nextPageToken} -> nextPageToken) (\s@ListWorkflowTypesResponse' {} a -> s {nextPageToken = a} :: ListWorkflowTypesResponse)

-- | The response's http status code.
listWorkflowTypesResponse_httpStatus :: Lens.Lens' ListWorkflowTypesResponse Prelude.Int
listWorkflowTypesResponse_httpStatus = Lens.lens (\ListWorkflowTypesResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowTypesResponse' {} a -> s {httpStatus = a} :: ListWorkflowTypesResponse)

-- | The list of workflow type information.
listWorkflowTypesResponse_typeInfos :: Lens.Lens' ListWorkflowTypesResponse [WorkflowTypeInfo]
listWorkflowTypesResponse_typeInfos = Lens.lens (\ListWorkflowTypesResponse' {typeInfos} -> typeInfos) (\s@ListWorkflowTypesResponse' {} a -> s {typeInfos = a} :: ListWorkflowTypesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListWorkflowTypesResponse
