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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registration tokens for the specified extension(s).
module Amazonka.CloudFormation.ListTypeRegistrations
  ( -- * Creating a Request
    ListTypeRegistrations (..),
    newListTypeRegistrations,

    -- * Request Lenses
    listTypeRegistrations_maxResults,
    listTypeRegistrations_nextToken,
    listTypeRegistrations_registrationStatusFilter,
    listTypeRegistrations_type,
    listTypeRegistrations_typeArn,
    listTypeRegistrations_typeName,

    -- * Destructuring the Response
    ListTypeRegistrationsResponse (..),
    newListTypeRegistrationsResponse,

    -- * Response Lenses
    listTypeRegistrationsResponse_nextToken,
    listTypeRegistrationsResponse_registrationTokenList,
    listTypeRegistrationsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTypeRegistrations' smart constructor.
data ListTypeRegistrations = ListTypeRegistrations'
  { -- | The maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous paginated request didn\'t return all the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current status of the extension registration request.
    --
    -- The default is @IN_PROGRESS@.
    registrationStatusFilter :: Prelude.Maybe RegistrationStatus,
    -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Prelude.Maybe RegistryType,
    -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listTypeRegistrations_maxResults' - The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'nextToken', 'listTypeRegistrations_nextToken' - If the previous paginated request didn\'t return all the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'registrationStatusFilter', 'listTypeRegistrations_registrationStatusFilter' - The current status of the extension registration request.
--
-- The default is @IN_PROGRESS@.
--
-- 'type'', 'listTypeRegistrations_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'typeArn', 'listTypeRegistrations_typeArn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'typeName', 'listTypeRegistrations_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
newListTypeRegistrations ::
  ListTypeRegistrations
newListTypeRegistrations =
  ListTypeRegistrations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registrationStatusFilter = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      typeName = Prelude.Nothing
    }

-- | The maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
listTypeRegistrations_maxResults :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Natural)
listTypeRegistrations_maxResults = Lens.lens (\ListTypeRegistrations' {maxResults} -> maxResults) (\s@ListTypeRegistrations' {} a -> s {maxResults = a} :: ListTypeRegistrations)

-- | If the previous paginated request didn\'t return all the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listTypeRegistrations_nextToken :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_nextToken = Lens.lens (\ListTypeRegistrations' {nextToken} -> nextToken) (\s@ListTypeRegistrations' {} a -> s {nextToken = a} :: ListTypeRegistrations)

-- | The current status of the extension registration request.
--
-- The default is @IN_PROGRESS@.
listTypeRegistrations_registrationStatusFilter :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe RegistrationStatus)
listTypeRegistrations_registrationStatusFilter = Lens.lens (\ListTypeRegistrations' {registrationStatusFilter} -> registrationStatusFilter) (\s@ListTypeRegistrations' {} a -> s {registrationStatusFilter = a} :: ListTypeRegistrations)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_type :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe RegistryType)
listTypeRegistrations_type = Lens.lens (\ListTypeRegistrations' {type'} -> type') (\s@ListTypeRegistrations' {} a -> s {type' = a} :: ListTypeRegistrations)

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_typeArn :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_typeArn = Lens.lens (\ListTypeRegistrations' {typeArn} -> typeArn) (\s@ListTypeRegistrations' {} a -> s {typeArn = a} :: ListTypeRegistrations)

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
listTypeRegistrations_typeName :: Lens.Lens' ListTypeRegistrations (Prelude.Maybe Prelude.Text)
listTypeRegistrations_typeName = Lens.lens (\ListTypeRegistrations' {typeName} -> typeName) (\s@ListTypeRegistrations' {} a -> s {typeName = a} :: ListTypeRegistrations)

instance Core.AWSRequest ListTypeRegistrations where
  type
    AWSResponse ListTypeRegistrations =
      ListTypeRegistrationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListTypeRegistrationsResult"
      ( \s h x ->
          ListTypeRegistrationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "RegistrationTokenList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypeRegistrations where
  hashWithSalt _salt ListTypeRegistrations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registrationStatusFilter
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeArn
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData ListTypeRegistrations where
  rnf ListTypeRegistrations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf registrationStatusFilter `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf typeArn `Prelude.seq`
              Prelude.rnf typeName

instance Data.ToHeaders ListTypeRegistrations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTypeRegistrations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTypeRegistrations where
  toQuery ListTypeRegistrations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListTypeRegistrations" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "RegistrationStatusFilter"
          Data.=: registrationStatusFilter,
        "Type" Data.=: type',
        "TypeArn" Data.=: typeArn,
        "TypeName" Data.=: typeName
      ]

-- | /See:/ 'newListTypeRegistrationsResponse' smart constructor.
data ListTypeRegistrationsResponse = ListTypeRegistrationsResponse'
  { -- | If the request doesn\'t return all the remaining results, @NextToken@ is
    -- set to a token. To retrieve the next set of results, call this action
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If the request returns all results, @NextToken@ is set to
    -- @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of extension registration tokens.
    --
    -- Use @ @@DescribeTypeRegistration@@ @ to return detailed information
    -- about a type registration request.
    registrationTokenList :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'listTypeRegistrationsResponse_nextToken' - If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
--
-- 'registrationTokenList', 'listTypeRegistrationsResponse_registrationTokenList' - A list of extension registration tokens.
--
-- Use @ @@DescribeTypeRegistration@@ @ to return detailed information
-- about a type registration request.
--
-- 'httpStatus', 'listTypeRegistrationsResponse_httpStatus' - The response's http status code.
newListTypeRegistrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypeRegistrationsResponse
newListTypeRegistrationsResponse pHttpStatus_ =
  ListTypeRegistrationsResponse'
    { nextToken =
        Prelude.Nothing,
      registrationTokenList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request doesn\'t return all the remaining results, @NextToken@ is
-- set to a token. To retrieve the next set of results, call this action
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If the request returns all results, @NextToken@ is set to
-- @null@.
listTypeRegistrationsResponse_nextToken :: Lens.Lens' ListTypeRegistrationsResponse (Prelude.Maybe Prelude.Text)
listTypeRegistrationsResponse_nextToken = Lens.lens (\ListTypeRegistrationsResponse' {nextToken} -> nextToken) (\s@ListTypeRegistrationsResponse' {} a -> s {nextToken = a} :: ListTypeRegistrationsResponse)

-- | A list of extension registration tokens.
--
-- Use @ @@DescribeTypeRegistration@@ @ to return detailed information
-- about a type registration request.
listTypeRegistrationsResponse_registrationTokenList :: Lens.Lens' ListTypeRegistrationsResponse (Prelude.Maybe [Prelude.Text])
listTypeRegistrationsResponse_registrationTokenList = Lens.lens (\ListTypeRegistrationsResponse' {registrationTokenList} -> registrationTokenList) (\s@ListTypeRegistrationsResponse' {} a -> s {registrationTokenList = a} :: ListTypeRegistrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTypeRegistrationsResponse_httpStatus :: Lens.Lens' ListTypeRegistrationsResponse Prelude.Int
listTypeRegistrationsResponse_httpStatus = Lens.lens (\ListTypeRegistrationsResponse' {httpStatus} -> httpStatus) (\s@ListTypeRegistrationsResponse' {} a -> s {httpStatus = a} :: ListTypeRegistrationsResponse)

instance Prelude.NFData ListTypeRegistrationsResponse where
  rnf ListTypeRegistrationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf registrationTokenList `Prelude.seq`
        Prelude.rnf httpStatus
