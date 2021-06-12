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
-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the
-- specified domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Network.AWS.Route53Domains.ListTagsForDomain
  ( -- * Creating a Request
    ListTagsForDomain (..),
    newListTagsForDomain,

    -- * Request Lenses
    listTagsForDomain_domainName,

    -- * Destructuring the Response
    ListTagsForDomainResponse (..),
    newListTagsForDomainResponse,

    -- * Response Lenses
    listTagsForDomainResponse_httpStatus,
    listTagsForDomainResponse_tagList,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The ListTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newListTagsForDomain' smart constructor.
data ListTagsForDomain = ListTagsForDomain'
  { -- | The domain for which you want to get a list of tags.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'listTagsForDomain_domainName' - The domain for which you want to get a list of tags.
newListTagsForDomain ::
  -- | 'domainName'
  Core.Text ->
  ListTagsForDomain
newListTagsForDomain pDomainName_ =
  ListTagsForDomain' {domainName = pDomainName_}

-- | The domain for which you want to get a list of tags.
listTagsForDomain_domainName :: Lens.Lens' ListTagsForDomain Core.Text
listTagsForDomain_domainName = Lens.lens (\ListTagsForDomain' {domainName} -> domainName) (\s@ListTagsForDomain' {} a -> s {domainName = a} :: ListTagsForDomain)

instance Core.AWSRequest ListTagsForDomain where
  type
    AWSResponse ListTagsForDomain =
      ListTagsForDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForDomainResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "TagList" Core..!@ Core.mempty)
      )

instance Core.Hashable ListTagsForDomain

instance Core.NFData ListTagsForDomain

instance Core.ToHeaders ListTagsForDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.ListTagsForDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForDomain where
  toJSON ListTagsForDomain' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath ListTagsForDomain where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForDomain where
  toQuery = Core.const Core.mempty

-- | The ListTagsForDomain response includes the following elements.
--
-- /See:/ 'newListTagsForDomainResponse' smart constructor.
data ListTagsForDomainResponse = ListTagsForDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of the tags that are associated with the specified domain.
    tagList :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTagsForDomainResponse_httpStatus' - The response's http status code.
--
-- 'tagList', 'listTagsForDomainResponse_tagList' - A list of the tags that are associated with the specified domain.
newListTagsForDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForDomainResponse
newListTagsForDomainResponse pHttpStatus_ =
  ListTagsForDomainResponse'
    { httpStatus =
        pHttpStatus_,
      tagList = Core.mempty
    }

-- | The response's http status code.
listTagsForDomainResponse_httpStatus :: Lens.Lens' ListTagsForDomainResponse Core.Int
listTagsForDomainResponse_httpStatus = Lens.lens (\ListTagsForDomainResponse' {httpStatus} -> httpStatus) (\s@ListTagsForDomainResponse' {} a -> s {httpStatus = a} :: ListTagsForDomainResponse)

-- | A list of the tags that are associated with the specified domain.
listTagsForDomainResponse_tagList :: Lens.Lens' ListTagsForDomainResponse [Tag]
listTagsForDomainResponse_tagList = Lens.lens (\ListTagsForDomainResponse' {tagList} -> tagList) (\s@ListTagsForDomainResponse' {} a -> s {tagList = a} :: ListTagsForDomainResponse) Core.. Lens._Coerce

instance Core.NFData ListTagsForDomainResponse
