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
-- Module      : Amazonka.SESV2.ListCustomVerificationEmailTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing custom verification email templates for your account
-- in the current Amazon Web Services Region.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom Using custom verification email templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.ListCustomVerificationEmailTemplates
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
    listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates,
    listCustomVerificationEmailTemplatesResponse_nextToken,
    listCustomVerificationEmailTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomVerificationEmailTemplatesResponse'
            Prelude.<$> ( x
                            Data..?> "CustomVerificationEmailTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomVerificationEmailTemplates
  where
  hashWithSalt
    _salt
    ListCustomVerificationEmailTemplates' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` pageSize

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplates
  where
  rnf ListCustomVerificationEmailTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance
  Data.ToHeaders
    ListCustomVerificationEmailTemplates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListCustomVerificationEmailTemplates
  where
  toPath =
    Prelude.const
      "/v2/email/custom-verification-email-templates"

instance
  Data.ToQuery
    ListCustomVerificationEmailTemplates
  where
  toQuery ListCustomVerificationEmailTemplates' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | The following elements are returned by the service.
--
-- /See:/ 'newListCustomVerificationEmailTemplatesResponse' smart constructor.
data ListCustomVerificationEmailTemplatesResponse = ListCustomVerificationEmailTemplatesResponse'
  { -- | A list of the custom verification email templates that exist in your
    -- account.
    customVerificationEmailTemplates :: Prelude.Maybe [CustomVerificationEmailTemplateMetadata],
    -- | A token indicating that there are additional custom verification email
    -- templates available to be listed. Pass this token to a subsequent call
    -- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
    -- verification email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'customVerificationEmailTemplates', 'listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates' - A list of the custom verification email templates that exist in your
-- account.
--
-- 'nextToken', 'listCustomVerificationEmailTemplatesResponse_nextToken' - A token indicating that there are additional custom verification email
-- templates available to be listed. Pass this token to a subsequent call
-- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
-- verification email templates.
--
-- 'httpStatus', 'listCustomVerificationEmailTemplatesResponse_httpStatus' - The response's http status code.
newListCustomVerificationEmailTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomVerificationEmailTemplatesResponse
newListCustomVerificationEmailTemplatesResponse
  pHttpStatus_ =
    ListCustomVerificationEmailTemplatesResponse'
      { customVerificationEmailTemplates =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of the custom verification email templates that exist in your
-- account.
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe [CustomVerificationEmailTemplateMetadata])
listCustomVerificationEmailTemplatesResponse_customVerificationEmailTemplates = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {customVerificationEmailTemplates} -> customVerificationEmailTemplates) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {customVerificationEmailTemplates = a} :: ListCustomVerificationEmailTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token indicating that there are additional custom verification email
-- templates available to be listed. Pass this token to a subsequent call
-- to @ListCustomVerificationEmailTemplates@ to retrieve the next 50 custom
-- verification email templates.
listCustomVerificationEmailTemplatesResponse_nextToken :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse (Prelude.Maybe Prelude.Text)
listCustomVerificationEmailTemplatesResponse_nextToken = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {nextToken} -> nextToken) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {nextToken = a} :: ListCustomVerificationEmailTemplatesResponse)

-- | The response's http status code.
listCustomVerificationEmailTemplatesResponse_httpStatus :: Lens.Lens' ListCustomVerificationEmailTemplatesResponse Prelude.Int
listCustomVerificationEmailTemplatesResponse_httpStatus = Lens.lens (\ListCustomVerificationEmailTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListCustomVerificationEmailTemplatesResponse' {} a -> s {httpStatus = a} :: ListCustomVerificationEmailTemplatesResponse)

instance
  Prelude.NFData
    ListCustomVerificationEmailTemplatesResponse
  where
  rnf ListCustomVerificationEmailTemplatesResponse' {..} =
    Prelude.rnf customVerificationEmailTemplates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
