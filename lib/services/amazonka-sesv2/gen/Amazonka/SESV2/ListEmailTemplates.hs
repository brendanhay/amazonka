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
-- Module      : Amazonka.SESV2.ListEmailTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the email templates present in your Amazon SES account in the
-- current Amazon Web Services Region.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.ListEmailTemplates
  ( -- * Creating a Request
    ListEmailTemplates (..),
    newListEmailTemplates,

    -- * Request Lenses
    listEmailTemplates_nextToken,
    listEmailTemplates_pageSize,

    -- * Destructuring the Response
    ListEmailTemplatesResponse (..),
    newListEmailTemplatesResponse,

    -- * Response Lenses
    listEmailTemplatesResponse_nextToken,
    listEmailTemplatesResponse_templatesMetadata,
    listEmailTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to list the email templates present in your Amazon
-- SES account in the current Amazon Web Services Region. For more
-- information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListEmailTemplates' smart constructor.
data ListEmailTemplates = ListEmailTemplates'
  { -- | A token returned from a previous call to @ListEmailTemplates@ to
    -- indicate the position in the list of email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to @ListEmailTemplates@.
    -- If the number of results is larger than the number you specified in this
    -- parameter, then the response includes a @NextToken@ element, which you
    -- can use to obtain additional results.
    --
    -- The value you specify has to be at least 1, and can be no more than 10.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEmailTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEmailTemplates_nextToken' - A token returned from a previous call to @ListEmailTemplates@ to
-- indicate the position in the list of email templates.
--
-- 'pageSize', 'listEmailTemplates_pageSize' - The number of results to show in a single call to @ListEmailTemplates@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 1, and can be no more than 10.
newListEmailTemplates ::
  ListEmailTemplates
newListEmailTemplates =
  ListEmailTemplates'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListEmailTemplates@ to
-- indicate the position in the list of email templates.
listEmailTemplates_nextToken :: Lens.Lens' ListEmailTemplates (Prelude.Maybe Prelude.Text)
listEmailTemplates_nextToken = Lens.lens (\ListEmailTemplates' {nextToken} -> nextToken) (\s@ListEmailTemplates' {} a -> s {nextToken = a} :: ListEmailTemplates)

-- | The number of results to show in a single call to @ListEmailTemplates@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 1, and can be no more than 10.
listEmailTemplates_pageSize :: Lens.Lens' ListEmailTemplates (Prelude.Maybe Prelude.Int)
listEmailTemplates_pageSize = Lens.lens (\ListEmailTemplates' {pageSize} -> pageSize) (\s@ListEmailTemplates' {} a -> s {pageSize = a} :: ListEmailTemplates)

instance Core.AWSRequest ListEmailTemplates where
  type
    AWSResponse ListEmailTemplates =
      ListEmailTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEmailTemplatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "TemplatesMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEmailTemplates where
  hashWithSalt _salt ListEmailTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListEmailTemplates where
  rnf ListEmailTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Core.ToHeaders ListEmailTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListEmailTemplates where
  toPath = Prelude.const "/v2/email/templates"

instance Core.ToQuery ListEmailTemplates where
  toQuery ListEmailTemplates' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "PageSize" Core.=: pageSize
      ]

-- | The following elements are returned by the service.
--
-- /See:/ 'newListEmailTemplatesResponse' smart constructor.
data ListEmailTemplatesResponse = ListEmailTemplatesResponse'
  { -- | A token indicating that there are additional email templates available
    -- to be listed. Pass this token to a subsequent @ListEmailTemplates@ call
    -- to retrieve the next 10 email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array the contains the name and creation time stamp for each template
    -- in your Amazon SES account.
    templatesMetadata :: Prelude.Maybe [EmailTemplateMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEmailTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEmailTemplatesResponse_nextToken' - A token indicating that there are additional email templates available
-- to be listed. Pass this token to a subsequent @ListEmailTemplates@ call
-- to retrieve the next 10 email templates.
--
-- 'templatesMetadata', 'listEmailTemplatesResponse_templatesMetadata' - An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
--
-- 'httpStatus', 'listEmailTemplatesResponse_httpStatus' - The response's http status code.
newListEmailTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEmailTemplatesResponse
newListEmailTemplatesResponse pHttpStatus_ =
  ListEmailTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      templatesMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token indicating that there are additional email templates available
-- to be listed. Pass this token to a subsequent @ListEmailTemplates@ call
-- to retrieve the next 10 email templates.
listEmailTemplatesResponse_nextToken :: Lens.Lens' ListEmailTemplatesResponse (Prelude.Maybe Prelude.Text)
listEmailTemplatesResponse_nextToken = Lens.lens (\ListEmailTemplatesResponse' {nextToken} -> nextToken) (\s@ListEmailTemplatesResponse' {} a -> s {nextToken = a} :: ListEmailTemplatesResponse)

-- | An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
listEmailTemplatesResponse_templatesMetadata :: Lens.Lens' ListEmailTemplatesResponse (Prelude.Maybe [EmailTemplateMetadata])
listEmailTemplatesResponse_templatesMetadata = Lens.lens (\ListEmailTemplatesResponse' {templatesMetadata} -> templatesMetadata) (\s@ListEmailTemplatesResponse' {} a -> s {templatesMetadata = a} :: ListEmailTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEmailTemplatesResponse_httpStatus :: Lens.Lens' ListEmailTemplatesResponse Prelude.Int
listEmailTemplatesResponse_httpStatus = Lens.lens (\ListEmailTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListEmailTemplatesResponse' {} a -> s {httpStatus = a} :: ListEmailTemplatesResponse)

instance Prelude.NFData ListEmailTemplatesResponse where
  rnf ListEmailTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templatesMetadata
      `Prelude.seq` Prelude.rnf httpStatus
