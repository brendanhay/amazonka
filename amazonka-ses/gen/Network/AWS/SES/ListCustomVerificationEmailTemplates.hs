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
-- Module      : Network.AWS.SES.ListCustomVerificationEmailTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing custom verification email templates for your account
-- in the current AWS Region.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListCustomVerificationEmailTemplates
  ( -- * Creating a Request
    ListCustomVerificationEmailTemplates (..),
    newListCustomVerificationEmailTemplates,

    -- * Request Lenses
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_maxResults,

    -- * Destructuring the Response
    ListCustomVerificationEmailTemplatesResponse (..),
    newListCustomVerificationEmailTemplatesResponse,

    -- * Response Lenses
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to list the existing custom verification email
-- templates for your account.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- /See:/ 'newListCustomVerificationEmailTemplates' smart constructor.
data ListCustomVerificationEmailTemplates = ListCustomVerificationEmailTemplates'
  { -- | An array the contains the name and creation time stamp for each template
    -- in your Amazon SES account.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of custom verification email templates to return.
    -- This value must be at least 1 and less than or equal to 50. If you do
    -- not specify a value, or if you specify a value less than 1 or greater
    -- than 50, the operation will return up to 50 results.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCustomVerificationEmailTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomVerificationEmailTemplates_nextToken' - An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
--
-- 'maxResults', 'listCustomVerificationEmailTemplates_maxResults' - The maximum number of custom verification email templates to return.
-- This value must be at least 1 and less than or equal to 50. If you do
-- not specify a value, or if you specify a value less than 1 or greater
-- than 50, the operation will return up to 50 results.
newListCustomVerificationEmailTemplates ::
  ListCustomVerificationEmailTemplates
newListCustomVerificationEmailTemplates =
  ListCustomVerificationEmailTemplates'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
listCustomVerificationEmailTemplates_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplates (Core.Maybe Core.Text)
listCustomVerificationEmailTemplates_nextToken = Lens.lens (\ListCustomVerificationEmailTemplates' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplates' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplates)

-- | The maximum number of custom verification email templates to return.
-- This value must be at least 1 and less than or equal to 50. If you do
-- not specify a value, or if you specify a value less than 1 or greater
-- than 50, the operation will return up to 50 results.
listCustomVerificationEmailTemplates_maxResults :: Lens.Lens' ListCustomVerificationEmailTemplates (Core.Maybe Core.Natural)
listCustomVerificationEmailTemplates_maxResults = Lens.lens (\ListCustomVerificationEmailTemplates' {maxResults} -> maxResults) (\s@ListCustomVerificationEmailTemplates' {} a -> s {maxResults = a} :: ListCustomVerificationEmailTemplates)

instance
  Core.AWSPager
    ListCustomVerificationEmailTemplates
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCustomVerificationEmailTemplates_nextToken
          Lens..~ rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    ListCustomVerificationEmailTemplates
  where
  type
    AWSResponse ListCustomVerificationEmailTemplates =
      ListCustomVerificationEmailTemplatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListCustomVerificationEmailTemplatesResult"
      ( \s h x ->
          ListCustomVerificationEmailTemplatesResponse'
            Core.<$> (x Core..@? "NextToken")
              Core.<*> ( x Core..@? "CustomVerificationEmailTemplates"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "member")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListCustomVerificationEmailTemplates

instance
  Core.NFData
    ListCustomVerificationEmailTemplates

instance
  Core.ToHeaders
    ListCustomVerificationEmailTemplates
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ListCustomVerificationEmailTemplates
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListCustomVerificationEmailTemplates
  where
  toQuery ListCustomVerificationEmailTemplates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ListCustomVerificationEmailTemplates" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | A paginated list of custom verification email templates.
--
-- /See:/ 'newListCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { -- | A token indicating that there are additional custom verification email
    -- templates available to be listed. Pass this token to a subsequent call
    -- to @ListTemplates@ to retrieve the next 50 custom verification email
    -- templates.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the custom verification email templates that exist in your
    -- account.
    customVerificationEmailTemplates :: Core.Maybe [CustomVerificationEmailTemplate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCustomVerificationEmailTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomVerificationEmailTemplatesResponse_nextToken' - A token indicating that there are additional custom verification email
-- templates available to be listed. Pass this token to a subsequent call
-- to @ListTemplates@ to retrieve the next 50 custom verification email
-- templates.
--
-- 'customVerificationEmailTemplates', 'listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates' - A list of the custom verification email templates that exist in your
-- account.
--
-- 'httpStatus', 'listCustomVerificationEmailTemplatesResponse_httpStatus' - The response's http status code.
newListCustomVerificationEmailTemplatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCustomVerificationEmailTemplatesResponse
newListCustomVerificationEmailTemplatesResponse
  pHttpStatus_ =
    ListCustomVerificationEmailTemplatesResponse'
      { nextToken =
          Core.Nothing,
        customVerificationEmailTemplates =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token indicating that there are additional custom verification email
-- templates available to be listed. Pass this token to a subsequent call
-- to @ListTemplates@ to retrieve the next 50 custom verification email
-- templates.
listCustomVerificationEmailTemplatesResponse_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Core.Maybe Core.Text)
listCustomVerificationEmailTemplatesResponse_nextToken = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplatesResponse)

-- | A list of the custom verification email templates that exist in your
-- account.
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Core.Maybe [CustomVerificationEmailTemplate])
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {customVerificationEmailTemplates} -> customVerificationEmailTemplates) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {customVerificationEmailTemplates = a} :: ListCustomVerificationEmailTemplatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCustomVerificationEmailTemplatesResponse_httpStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Core.Int
listCustomVerificationEmailTemplatesResponse_httpStatus = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {httpStatus = a} :: ListCustomVerificationEmailTemplatesResponse)

instance
  Core.NFData
    ListCustomVerificationEmailTemplatesResponse
