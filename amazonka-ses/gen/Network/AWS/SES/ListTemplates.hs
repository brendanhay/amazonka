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
-- Module      : Network.AWS.SES.ListTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the email templates present in your Amazon SES account in the
-- current AWS Region.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListTemplates
  ( -- * Creating a Request
    ListTemplates (..),
    newListTemplates,

    -- * Request Lenses
    listTemplates_nextToken,
    listTemplates_maxItems,

    -- * Destructuring the Response
    ListTemplatesResponse (..),
    newListTemplatesResponse,

    -- * Response Lenses
    listTemplatesResponse_nextToken,
    listTemplatesResponse_templatesMetadata,
    listTemplatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | A token returned from a previous call to @ListTemplates@ to indicate the
    -- position in the list of email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of templates to return. This value must be at least 1
    -- and less than or equal to 10. If you do not specify a value, or if you
    -- specify a value less than 1 or greater than 10, the operation will
    -- return up to 10 results.
    maxItems :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplates_nextToken' - A token returned from a previous call to @ListTemplates@ to indicate the
-- position in the list of email templates.
--
-- 'maxItems', 'listTemplates_maxItems' - The maximum number of templates to return. This value must be at least 1
-- and less than or equal to 10. If you do not specify a value, or if you
-- specify a value less than 1 or greater than 10, the operation will
-- return up to 10 results.
newListTemplates ::
  ListTemplates
newListTemplates =
  ListTemplates'
    { nextToken = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListTemplates@ to indicate the
-- position in the list of email templates.
listTemplates_nextToken :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Text)
listTemplates_nextToken = Lens.lens (\ListTemplates' {nextToken} -> nextToken) (\s@ListTemplates' {} a -> s {nextToken = a} :: ListTemplates)

-- | The maximum number of templates to return. This value must be at least 1
-- and less than or equal to 10. If you do not specify a value, or if you
-- specify a value less than 1 or greater than 10, the operation will
-- return up to 10 results.
listTemplates_maxItems :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Int)
listTemplates_maxItems = Lens.lens (\ListTemplates' {maxItems} -> maxItems) (\s@ListTemplates' {} a -> s {maxItems = a} :: ListTemplates)

instance Core.AWSPager ListTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplatesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplatesResponse_templatesMetadata
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTemplates_nextToken
          Lens..~ rs
          Lens.^? listTemplatesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTemplates where
  type
    AWSResponse ListTemplates =
      ListTemplatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTemplatesResult"
      ( \s h x ->
          ListTemplatesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "TemplatesMetadata"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplates

instance Prelude.NFData ListTemplates

instance Core.ToHeaders ListTemplates where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListTemplates" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | A token indicating that there are additional email templates available
    -- to be listed. Pass this token to a subsequent call to @ListTemplates@ to
    -- retrieve the next 50 email templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array the contains the name and creation time stamp for each template
    -- in your Amazon SES account.
    templatesMetadata :: Prelude.Maybe [TemplateMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplatesResponse_nextToken' - A token indicating that there are additional email templates available
-- to be listed. Pass this token to a subsequent call to @ListTemplates@ to
-- retrieve the next 50 email templates.
--
-- 'templatesMetadata', 'listTemplatesResponse_templatesMetadata' - An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
--
-- 'httpStatus', 'listTemplatesResponse_httpStatus' - The response's http status code.
newListTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplatesResponse
newListTemplatesResponse pHttpStatus_ =
  ListTemplatesResponse'
    { nextToken = Prelude.Nothing,
      templatesMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token indicating that there are additional email templates available
-- to be listed. Pass this token to a subsequent call to @ListTemplates@ to
-- retrieve the next 50 email templates.
listTemplatesResponse_nextToken :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe Prelude.Text)
listTemplatesResponse_nextToken = Lens.lens (\ListTemplatesResponse' {nextToken} -> nextToken) (\s@ListTemplatesResponse' {} a -> s {nextToken = a} :: ListTemplatesResponse)

-- | An array the contains the name and creation time stamp for each template
-- in your Amazon SES account.
listTemplatesResponse_templatesMetadata :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe [TemplateMetadata])
listTemplatesResponse_templatesMetadata = Lens.lens (\ListTemplatesResponse' {templatesMetadata} -> templatesMetadata) (\s@ListTemplatesResponse' {} a -> s {templatesMetadata = a} :: ListTemplatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTemplatesResponse_httpStatus :: Lens.Lens' ListTemplatesResponse Prelude.Int
listTemplatesResponse_httpStatus = Lens.lens (\ListTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListTemplatesResponse' {} a -> s {httpStatus = a} :: ListTemplatesResponse)

instance Prelude.NFData ListTemplatesResponse
