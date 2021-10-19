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
-- Module      : Network.AWS.AppStream.DescribeUserStackAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the UserStackAssociation objects. You
-- must specify either or both of the following:
--
-- -   The stack name
--
-- -   The user name (email address of the user associated with the stack)
--     and the authentication type for the user
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUserStackAssociations
  ( -- * Creating a Request
    DescribeUserStackAssociations (..),
    newDescribeUserStackAssociations,

    -- * Request Lenses
    describeUserStackAssociations_userName,
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_authenticationType,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociations_stackName,

    -- * Destructuring the Response
    DescribeUserStackAssociationsResponse (..),
    newDescribeUserStackAssociationsResponse,

    -- * Response Lenses
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserStackAssociations' smart constructor.
data DescribeUserStackAssociations = DescribeUserStackAssociations'
  { -- | The email address of the user who is associated with the stack.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The authentication type for the user who is associated with the stack.
    -- You must specify USERPOOL.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stack that is associated with the user.
    stackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserStackAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'describeUserStackAssociations_userName' - The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
--
-- 'nextToken', 'describeUserStackAssociations_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'authenticationType', 'describeUserStackAssociations_authenticationType' - The authentication type for the user who is associated with the stack.
-- You must specify USERPOOL.
--
-- 'maxResults', 'describeUserStackAssociations_maxResults' - The maximum size of each page of results.
--
-- 'stackName', 'describeUserStackAssociations_stackName' - The name of the stack that is associated with the user.
newDescribeUserStackAssociations ::
  DescribeUserStackAssociations
newDescribeUserStackAssociations =
  DescribeUserStackAssociations'
    { userName =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
describeUserStackAssociations_userName :: Lens.Lens' DescribeUserStackAssociations (Prelude.Maybe Prelude.Text)
describeUserStackAssociations_userName = Lens.lens (\DescribeUserStackAssociations' {userName} -> userName) (\s@DescribeUserStackAssociations' {} a -> s {userName = a} :: DescribeUserStackAssociations) Prelude.. Lens.mapping Core._Sensitive

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUserStackAssociations_nextToken :: Lens.Lens' DescribeUserStackAssociations (Prelude.Maybe Prelude.Text)
describeUserStackAssociations_nextToken = Lens.lens (\DescribeUserStackAssociations' {nextToken} -> nextToken) (\s@DescribeUserStackAssociations' {} a -> s {nextToken = a} :: DescribeUserStackAssociations)

-- | The authentication type for the user who is associated with the stack.
-- You must specify USERPOOL.
describeUserStackAssociations_authenticationType :: Lens.Lens' DescribeUserStackAssociations (Prelude.Maybe AuthenticationType)
describeUserStackAssociations_authenticationType = Lens.lens (\DescribeUserStackAssociations' {authenticationType} -> authenticationType) (\s@DescribeUserStackAssociations' {} a -> s {authenticationType = a} :: DescribeUserStackAssociations)

-- | The maximum size of each page of results.
describeUserStackAssociations_maxResults :: Lens.Lens' DescribeUserStackAssociations (Prelude.Maybe Prelude.Natural)
describeUserStackAssociations_maxResults = Lens.lens (\DescribeUserStackAssociations' {maxResults} -> maxResults) (\s@DescribeUserStackAssociations' {} a -> s {maxResults = a} :: DescribeUserStackAssociations)

-- | The name of the stack that is associated with the user.
describeUserStackAssociations_stackName :: Lens.Lens' DescribeUserStackAssociations (Prelude.Maybe Prelude.Text)
describeUserStackAssociations_stackName = Lens.lens (\DescribeUserStackAssociations' {stackName} -> stackName) (\s@DescribeUserStackAssociations' {} a -> s {stackName = a} :: DescribeUserStackAssociations)

instance Core.AWSPager DescribeUserStackAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUserStackAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUserStackAssociationsResponse_userStackAssociations
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUserStackAssociations_nextToken
          Lens..~ rs
          Lens.^? describeUserStackAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeUserStackAssociations
  where
  type
    AWSResponse DescribeUserStackAssociations =
      DescribeUserStackAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserStackAssociationsResponse'
            Prelude.<$> (x Core..?> "UserStackAssociations")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeUserStackAssociations

instance Prelude.NFData DescribeUserStackAssociations

instance Core.ToHeaders DescribeUserStackAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeUserStackAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUserStackAssociations where
  toJSON DescribeUserStackAssociations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserName" Core..=) Prelude.<$> userName,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("AuthenticationType" Core..=)
              Prelude.<$> authenticationType,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StackName" Core..=) Prelude.<$> stackName
          ]
      )

instance Core.ToPath DescribeUserStackAssociations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUserStackAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserStackAssociationsResponse' smart constructor.
data DescribeUserStackAssociationsResponse = DescribeUserStackAssociationsResponse'
  { -- | The UserStackAssociation objects.
    userStackAssociations :: Prelude.Maybe (Prelude.NonEmpty UserStackAssociation),
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserStackAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userStackAssociations', 'describeUserStackAssociationsResponse_userStackAssociations' - The UserStackAssociation objects.
--
-- 'nextToken', 'describeUserStackAssociationsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'describeUserStackAssociationsResponse_httpStatus' - The response's http status code.
newDescribeUserStackAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserStackAssociationsResponse
newDescribeUserStackAssociationsResponse pHttpStatus_ =
  DescribeUserStackAssociationsResponse'
    { userStackAssociations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The UserStackAssociation objects.
describeUserStackAssociationsResponse_userStackAssociations :: Lens.Lens' DescribeUserStackAssociationsResponse (Prelude.Maybe (Prelude.NonEmpty UserStackAssociation))
describeUserStackAssociationsResponse_userStackAssociations = Lens.lens (\DescribeUserStackAssociationsResponse' {userStackAssociations} -> userStackAssociations) (\s@DescribeUserStackAssociationsResponse' {} a -> s {userStackAssociations = a} :: DescribeUserStackAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUserStackAssociationsResponse_nextToken :: Lens.Lens' DescribeUserStackAssociationsResponse (Prelude.Maybe Prelude.Text)
describeUserStackAssociationsResponse_nextToken = Lens.lens (\DescribeUserStackAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeUserStackAssociationsResponse' {} a -> s {nextToken = a} :: DescribeUserStackAssociationsResponse)

-- | The response's http status code.
describeUserStackAssociationsResponse_httpStatus :: Lens.Lens' DescribeUserStackAssociationsResponse Prelude.Int
describeUserStackAssociationsResponse_httpStatus = Lens.lens (\DescribeUserStackAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeUserStackAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeUserStackAssociationsResponse)

instance
  Prelude.NFData
    DescribeUserStackAssociationsResponse
