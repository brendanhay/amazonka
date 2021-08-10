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
-- Module      : Network.AWS.SESv2.ListCustomVerificationEmailTemplates
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
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-verify-address-custom.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.ListCustomVerificationEmailTemplates
  ( -- * Creating a Request
    ListCustomVerificationEmailTemplates (..),
    newListCustomVerificationEmailTemplates,

    -- * Request Lenses
    listCustomVerificationEmailTemplates_nextToken,
    listCustomVerificationEmailTemplates_pageSize,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to list the existing custom verification email
-- templates for your account.
--
-- /See:/ 'newListCustomVerificationEmailTemplates' smart constructor.
data ListCustomVerificationEmailTemplates = ListCustomVerificationEmailTemplates'
  { -- | A token returned from a previous call to
    -- @ListCustomVerificationEmailTemplates@ to indicate the position in the
    -- list of custom verification email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to
    -- @ListCustomVerificationEmailTemplates@. If the number of results is
    -- larger than the number you specified in this parameter, then the
    -- response includes a @NextToken@ element, which you can use to obtain
    -- additional results.
    --
    -- The value you specify has to be at least 1, and can be no more than 50.
    pageSize :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'listCustomVerificationEmailTemplates_nextToken' - A token returned from a previous call to
-- @ListCustomVerificationEmailTemplates@ to indicate the position in the
-- list of custom verification email templates.
--
-- 'pageSize', 'listCustomVerificationEmailTemplates_pageSize' - The number of results to show in a single call to
-- @ListCustomVerificationEmailTemplates@. If the number of results is
-- larger than the number you specified in this parameter, then the
-- response includes a @NextToken@ element, which you can use to obtain
-- additional results.
--
-- The value you specify has to be at least 1, and can be no more than 50.
newListCustomVerificationEmailTemplates ::
  ListCustomVerificationEmailTemplates
newListCustomVerificationEmailTemplates =
  ListCustomVerificationEmailTemplates'
    { nextToken =
        Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to
-- @ListCustomVerificationEmailTemplates@ to indicate the position in the
-- list of custom verification email templates.
listCustomVerificationEmailTemplates_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplates (Prelude.Maybe Prelude.Text)
listCustomVerificationEmailTemplates_nextToken = Lens.lens (\ListCustomVerificationEmailTemplates' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplates' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplates)

-- | The number of results to show in a single call to
-- @ListCustomVerificationEmailTemplates@. If the number of results is
-- larger than the number you specified in this parameter, then the
-- response includes a @NextToken@ element, which you can use to obtain
-- additional results.
--
-- The value you specify has to be at least 1, and can be no more than 50.
listCustomVerificationEmailTemplates_pageSize :: Lens.Lens' ListCustomVerificationEmailTemplates (Prelude.Maybe Prelude.Int)
listCustomVerificationEmailTemplates_pageSize = Lens.lens (\ListCustomVerificationEmailTemplates' {pageSize} -> pageSize) (\s@ListCustomVerificationEmailTemplates' {} a -> s {pageSize = a} :: ListCustomVerificationEmailTemplates)

instance
  Core.AWSRequest
    ListCustomVerificationEmailTemplates
  where
  type
    AWSResponse ListCustomVerificationEmailTemplates =
      ListCustomVerificationEmailTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomVerificationEmailTemplatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "CustomVerificationEmailTemplates"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomVerificationEmailTemplates

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplates

instance
  Core.ToHeaders
    ListCustomVerificationEmailTemplates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    ListCustomVerificationEmailTemplates
  where
  toPath =
    Prelude.const
      "/v2/email/custom-verification-email-templates"

instance
  Core.ToQuery
    ListCustomVerificationEmailTemplates
  where
  toQuery ListCustomVerificationEmailTemplates' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize
      ]

-- | The following elements are returned by the service.
--
-- /See:/ 'newListCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { -- | A token indicating that there are additional custom verification email
    -- templates available to be listed. Pass this token to a subsequent call
    -- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
    -- verification email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the custom verification email templates that exist in your
    -- account.
    customVerificationEmailTemplates :: Prelude.Maybe [CustomVerificationEmailTemplateMetadata],
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
-- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
-- verification email templates.
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
-- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
-- verification email templates.
listCustomVerificationEmailTemplatesResponse_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe Prelude.Text)
listCustomVerificationEmailTemplatesResponse_nextToken = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplatesResponse)

-- | A list of the custom verification email templates that exist in your
-- account.
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe [CustomVerificationEmailTemplateMetadata])
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {customVerificationEmailTemplates} -> customVerificationEmailTemplates) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {customVerificationEmailTemplates = a} :: ListCustomVerificationEmailTemplatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCustomVerificationEmailTemplatesResponse_httpStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Prelude.Int
listCustomVerificationEmailTemplatesResponse_httpStatus = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {httpStatus = a} :: ListCustomVerificationEmailTemplatesResponse)

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplatesResponse
