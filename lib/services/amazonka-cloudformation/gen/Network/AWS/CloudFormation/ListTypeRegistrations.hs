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
-- Module      : Amazonka.CloudFormation.ListTypeRegistrations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registration tokens for the specified extension(s).
module Amazonka.CloudFormation.ListTypeRegistrations
  ( -- * Creating a Request
    ListTypeRegistrations (..),
    newListTypeRegistrations,

    -- * Request Lenses
    listTypeRegistrations_typeName,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_nextToken,
    listTypeRegistrations_typeArn,
    listTypeRegistrations_type,
    listTypeRegistrations_maxResults,

    -- * Destructuring the Response
    ListTypeRegistrationsResponse (..),
    newListTypeRegistrationsResponse,

    -- * Response Lenses
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypeRegistrations' smart constructor.
data ListTypeRegistrations = ListTypeRegistrations'
  { -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the extension registration request.
    --
    -- The default is @IN_PROGRESS@.
    registrationStatusFilter :: Prelude.Maybe RegistrationStatus,
    -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Prelude.Maybe RegistryType,
    -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypeRegistrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'listTypeRegistrations_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'registrationStatusFilter', 'listTypeRegistrations_registrationStatusFilter' - The current status of the extension registration request.
--
-- The default is @IN_PROGRESS@.
--
-- 'nextToken', 'listTypeRegistrations_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'typeArn', 'listTypeRegistrations_typeArn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'type'', 'listTypeRegistrations_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'maxResults', 'listTypeRegistrations_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
newListTypeRegistrations ::
  ListTypeRegistrations
newListTypeRegistrations =
  ListTypeRegistrations'
    { typeName = Prelude.Nothing,
      registrationStatusFilter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_typeName :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_typeName = Lens.lens (\ListTypeRegistrations' {typeName} -> typeName) (\s@ListTypeRegistrations' {} a -> s {typeName = a} :: ListTypeRegistrations)

-- | The current status of the extension registration request.
--
-- The default is @IN_PROGRESS@.
listTypeRegistrations_registrationStatusFilter :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe RegistrationStatus)
listTypeRegistrations_registrationStatusFilter = Lens.lens (\ListTypeRegistrations' {registrationStatusFilter} -> registrationStatusFilter) (\s@ListTypeRegistrations' {} a -> s {registrationStatusFilter = a} :: ListTypeRegistrations)

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypeRegistrations_nextToken :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_nextToken = Lens.lens (\ListTypeRegistrations' {nextToken} -> nextToken) (\s@ListTypeRegistrations' {} a -> s {nextToken = a} :: ListTypeRegistrations)

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_typeArn :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_typeArn = Lens.lens (\ListTypeRegistrations' {typeArn} -> typeArn) (\s@ListTypeRegistrations' {} a -> s {typeArn = a} :: ListTypeRegistrations)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_type :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe RegistryType)
listTypeRegistrations_type = Lens.lens (\ListTypeRegistrations' {type'} -> type') (\s@ListTypeRegistrations' {} a -> s {type' = a} :: ListTypeRegistrations)

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypeRegistrations_maxResults :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Natural)
listTypeRegistrations_maxResults = Lens.lens (\ListTypeRegistrations' {maxResults} -> maxResults) (\s@ListTypeRegistrations' {} a -> s {maxResults = a} :: ListTypeRegistrations)

instance Core.AWSRequest ListTypeRegistrations where
  type
    AWSResponse ListTypeRegistrations =
      ListTypeRegistrationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTypeRegistrationsResult"
      ( \s h x ->
          ListTypeRegistrationsResponse'
            Prelude.<$> ( x Core..@? "RegistrationTokenList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypeRegistrations

instance Prelude.NFData ListTypeRegistrations

instance Core.ToHeaders ListTypeRegistrations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTypeRegistrations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTypeRegistrations where
  toQuery ListTypeRegistrations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListTypeRegistrations" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "TypeName" Core.=: typeName,
        "RegistrationStatusFilter"
          Core.=: registrationStatusFilter,
        "NextToken" Core.=: nextToken,
        "TypeArn" Core.=: typeArn,
        "Type" Core.=: type',
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTypeRegistrationsResponse' smart constructor.
data ListTypeRegistrationsResponse = ListTypeRegistrationsResponse'
  { -- | A list of extension registration tokens.
    --
    -- Use @ DescribeTypeRegistration @ to return detailed information about a
    -- type registration request.
    registrationTokenList :: Prelude.Maybe [Prelude.Text],
    -- | If the request doesn\'t return all of the remaining results, @NextToken@
    -- is set to a token. To retrieve the next set of results, call this action
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If the request returns all results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypeRegistrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationTokenList', 'listTypeRegistrationsResponse_registrationTokenList' - A list of extension registration tokens.
--
-- Use @ DescribeTypeRegistration @ to return detailed information about a
-- type registration request.
--
-- 'nextToken', 'listTypeRegistrationsResponse_nextToken' - If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
--
-- 'httpStatus', 'listTypeRegistrationsResponse_httpStatus' - The response's http status code.
newListTypeRegistrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypeRegistrationsResponse
newListTypeRegistrationsResponse pHttpStatus_ =
  ListTypeRegistrationsResponse'
    { registrationTokenList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of extension registration tokens.
--
-- Use @ DescribeTypeRegistration @ to return detailed information about a
-- type registration request.
listTypeRegistrationsResponse_registrationTokenList :: Lens.Lens' ListTypeRegistrationsResponse (Prelude.Maybe [Prelude.Text])
listTypeRegistrationsResponse_registrationTokenList = Lens.lens (\ListTypeRegistrationsResponse' {registrationTokenList} -> registrationTokenList) (\s@ListTypeRegistrationsResponse' {} a -> s {registrationTokenList = a} :: ListTypeRegistrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the request doesn\'t return all of the remaining results, @NextToken@
-- is set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypeRegistrationsResponse_nextToken :: Lens.Lens' ListTypeRegistrationsResponse (Prelude.Maybe Prelude.Text)
listTypeRegistrationsResponse_nextToken = Lens.lens (\ListTypeRegistrationsResponse' {nextToken} -> nextToken) (\s@ListTypeRegistrationsResponse' {} a -> s {nextToken = a} :: ListTypeRegistrationsResponse)

-- | The response's http status code.
listTypeRegistrationsResponse_httpStatus :: Lens.Lens' ListTypeRegistrationsResponse Prelude.Int
listTypeRegistrationsResponse_httpStatus = Lens.lens (\ListTypeRegistrationsResponse' {httpStatus} -> httpStatus) (\s@ListTypeRegistrationsResponse' {} a -> s {httpStatus = a} :: ListTypeRegistrationsResponse)

instance Prelude.NFData ListTypeRegistrationsResponse
