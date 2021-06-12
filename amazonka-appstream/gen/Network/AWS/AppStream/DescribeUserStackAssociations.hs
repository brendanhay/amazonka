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
    describeUserStackAssociations_nextToken,
    describeUserStackAssociations_stackName,
    describeUserStackAssociations_maxResults,
    describeUserStackAssociations_userName,
    describeUserStackAssociations_authenticationType,

    -- * Destructuring the Response
    DescribeUserStackAssociationsResponse (..),
    newDescribeUserStackAssociationsResponse,

    -- * Response Lenses
    describeUserStackAssociationsResponse_nextToken,
    describeUserStackAssociationsResponse_userStackAssociations,
    describeUserStackAssociationsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserStackAssociations' smart constructor.
data DescribeUserStackAssociations = DescribeUserStackAssociations'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the stack that is associated with the user.
    stackName :: Core.Maybe Core.Text,
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The email address of the user who is associated with the stack.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The authentication type for the user who is associated with the stack.
    -- You must specify USERPOOL.
    authenticationType :: Core.Maybe AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserStackAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUserStackAssociations_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'stackName', 'describeUserStackAssociations_stackName' - The name of the stack that is associated with the user.
--
-- 'maxResults', 'describeUserStackAssociations_maxResults' - The maximum size of each page of results.
--
-- 'userName', 'describeUserStackAssociations_userName' - The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'describeUserStackAssociations_authenticationType' - The authentication type for the user who is associated with the stack.
-- You must specify USERPOOL.
newDescribeUserStackAssociations ::
  DescribeUserStackAssociations
newDescribeUserStackAssociations =
  DescribeUserStackAssociations'
    { nextToken =
        Core.Nothing,
      stackName = Core.Nothing,
      maxResults = Core.Nothing,
      userName = Core.Nothing,
      authenticationType = Core.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeUserStackAssociations_nextToken :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Core.Text)
describeUserStackAssociations_nextToken = Lens.lens (\DescribeUserStackAssociations' {nextToken} -> nextToken) (\s@DescribeUserStackAssociations' {} a -> s {nextToken = a} :: DescribeUserStackAssociations)

-- | The name of the stack that is associated with the user.
describeUserStackAssociations_stackName :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Core.Text)
describeUserStackAssociations_stackName = Lens.lens (\DescribeUserStackAssociations' {stackName} -> stackName) (\s@DescribeUserStackAssociations' {} a -> s {stackName = a} :: DescribeUserStackAssociations)

-- | The maximum size of each page of results.
describeUserStackAssociations_maxResults :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Core.Natural)
describeUserStackAssociations_maxResults = Lens.lens (\DescribeUserStackAssociations' {maxResults} -> maxResults) (\s@DescribeUserStackAssociations' {} a -> s {maxResults = a} :: DescribeUserStackAssociations)

-- | The email address of the user who is associated with the stack.
--
-- Users\' email addresses are case-sensitive.
describeUserStackAssociations_userName :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe Core.Text)
describeUserStackAssociations_userName = Lens.lens (\DescribeUserStackAssociations' {userName} -> userName) (\s@DescribeUserStackAssociations' {} a -> s {userName = a} :: DescribeUserStackAssociations) Core.. Lens.mapping Core._Sensitive

-- | The authentication type for the user who is associated with the stack.
-- You must specify USERPOOL.
describeUserStackAssociations_authenticationType :: Lens.Lens' DescribeUserStackAssociations (Core.Maybe AuthenticationType)
describeUserStackAssociations_authenticationType = Lens.lens (\DescribeUserStackAssociations' {authenticationType} -> authenticationType) (\s@DescribeUserStackAssociations' {} a -> s {authenticationType = a} :: DescribeUserStackAssociations)

instance Core.AWSPager DescribeUserStackAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUserStackAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUserStackAssociationsResponse_userStackAssociations
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeUserStackAssociations_nextToken
          Lens..~ rs
          Lens.^? describeUserStackAssociationsResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "UserStackAssociations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserStackAssociations

instance Core.NFData DescribeUserStackAssociations

instance Core.ToHeaders DescribeUserStackAssociations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeUserStackAssociations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserStackAssociations where
  toJSON DescribeUserStackAssociations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("StackName" Core..=) Core.<$> stackName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("UserName" Core..=) Core.<$> userName,
            ("AuthenticationType" Core..=)
              Core.<$> authenticationType
          ]
      )

instance Core.ToPath DescribeUserStackAssociations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserStackAssociations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserStackAssociationsResponse' smart constructor.
data DescribeUserStackAssociationsResponse = DescribeUserStackAssociationsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The UserStackAssociation objects.
    userStackAssociations :: Core.Maybe (Core.NonEmpty UserStackAssociation),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserStackAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeUserStackAssociationsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'userStackAssociations', 'describeUserStackAssociationsResponse_userStackAssociations' - The UserStackAssociation objects.
--
-- 'httpStatus', 'describeUserStackAssociationsResponse_httpStatus' - The response's http status code.
newDescribeUserStackAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserStackAssociationsResponse
newDescribeUserStackAssociationsResponse pHttpStatus_ =
  DescribeUserStackAssociationsResponse'
    { nextToken =
        Core.Nothing,
      userStackAssociations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeUserStackAssociationsResponse_nextToken :: Lens.Lens' DescribeUserStackAssociationsResponse (Core.Maybe Core.Text)
describeUserStackAssociationsResponse_nextToken = Lens.lens (\DescribeUserStackAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeUserStackAssociationsResponse' {} a -> s {nextToken = a} :: DescribeUserStackAssociationsResponse)

-- | The UserStackAssociation objects.
describeUserStackAssociationsResponse_userStackAssociations :: Lens.Lens' DescribeUserStackAssociationsResponse (Core.Maybe (Core.NonEmpty UserStackAssociation))
describeUserStackAssociationsResponse_userStackAssociations = Lens.lens (\DescribeUserStackAssociationsResponse' {userStackAssociations} -> userStackAssociations) (\s@DescribeUserStackAssociationsResponse' {} a -> s {userStackAssociations = a} :: DescribeUserStackAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeUserStackAssociationsResponse_httpStatus :: Lens.Lens' DescribeUserStackAssociationsResponse Core.Int
describeUserStackAssociationsResponse_httpStatus = Lens.lens (\DescribeUserStackAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeUserStackAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeUserStackAssociationsResponse)

instance
  Core.NFData
    DescribeUserStackAssociationsResponse
