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
-- Module      : Amazonka.SES.ListCustomVerificationEmailTemplates
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
module Amazonka.SES.ListCustomVerificationEmailTemplates
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of custom verification email templates to return.
    -- This value must be at least 1 and less than or equal to 50. If you do
    -- not specify a value, or if you specify a value less than 1 or greater
    -- than 50, the operation will return up to 50 results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
listCustomVerificationEmailTemplates_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplates (Prelude.Maybe Prelude.Text)
listCustomVerificationEmailTemplates_nextToken = Lens.lens (\ListCustomVerificationEmailTemplates' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplates' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplates)

-- | The maximum number of custom verification email templates to return.
-- This value must be at least 1 and less than or equal to 50. If you do
-- not specify a value, or if you specify a value less than 1 or greater
-- than 50, the operation will return up to 50 results.
listCustomVerificationEmailTemplates_maxResults :: Lens.Lens' ListCustomVerificationEmailTemplates (Prelude.Maybe Prelude.Natural)
listCustomVerificationEmailTemplates_maxResults = Lens.lens (\ListCustomVerificationEmailTemplates' {maxResults} -> maxResults) (\s@ListCustomVerificationEmailTemplates' {} a -> s {maxResults = a} :: ListCustomVerificationEmailTemplates)

instance
  Core.AWSPager
    ListCustomVerificationEmailTemplates
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomVerificationEmailTemplates_nextToken
          Lens..~ rs
            Lens.^? listCustomVerificationEmailTemplatesResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "NextToken")
              Prelude.<*> ( x Core..@? "CustomVerificationEmailTemplates"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "member")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomVerificationEmailTemplates
  where
  hashWithSalt
    _salt
    ListCustomVerificationEmailTemplates' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplates
  where
  rnf ListCustomVerificationEmailTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListCustomVerificationEmailTemplates
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListCustomVerificationEmailTemplates
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListCustomVerificationEmailTemplates
  where
  toQuery ListCustomVerificationEmailTemplates' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ListCustomVerificationEmailTemplates" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the custom verification email templates that exist in your
    -- account.
    customVerificationEmailTemplates :: Prelude.Maybe [CustomVerificationEmailTemplate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListCustomVerificationEmailTemplatesResponse
newListCustomVerificationEmailTemplatesResponse
  pHttpStatus_ =
    ListCustomVerificationEmailTemplatesResponse'
      { nextToken =
          Prelude.Nothing,
        customVerificationEmailTemplates =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token indicating that there are additional custom verification email
-- templates available to be listed. Pass this token to a subsequent call
-- to @ListTemplates@ to retrieve the next 50 custom verification email
-- templates.
listCustomVerificationEmailTemplatesResponse_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe Prelude.Text)
listCustomVerificationEmailTemplatesResponse_nextToken = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplatesResponse)

-- | A list of the custom verification email templates that exist in your
-- account.
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe [CustomVerificationEmailTemplate])
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {customVerificationEmailTemplates} -> customVerificationEmailTemplates) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {customVerificationEmailTemplates = a} :: ListCustomVerificationEmailTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomVerificationEmailTemplatesResponse_httpStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Prelude.Int
listCustomVerificationEmailTemplatesResponse_httpStatus = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {httpStatus = a} :: ListCustomVerificationEmailTemplatesResponse)

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplatesResponse
  where
  rnf ListCustomVerificationEmailTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf customVerificationEmailTemplates
      `Prelude.seq` Prelude.rnf httpStatus
