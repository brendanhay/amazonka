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
-- Module      : Amazonka.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the
-- specified domain.
--
-- All tag operations are eventually consistent; subsequent operations
-- might not immediately represent all issued operations.
module Amazonka.Route53Domains.ListTagsForDomain
  ( -- * Creating a Request
    ListTagsForDomain (..),
    newListTagsForDomain,

    -- * Request Lenses
    listTagsForDomain_domainName,

    -- * Destructuring the Response
    ListTagsForDomainResponse (..),
    newListTagsForDomainResponse,

    -- * Response Lenses
    listTagsForDomainResponse_tagList,
    listTagsForDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The ListTagsForDomainRequest includes the following elements.
--
-- /See:/ 'newListTagsForDomain' smart constructor.
data ListTagsForDomain = ListTagsForDomain'
  { -- | The domain for which you want to get a list of tags.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListTagsForDomain
newListTagsForDomain pDomainName_ =
  ListTagsForDomain' {domainName = pDomainName_}

-- | The domain for which you want to get a list of tags.
listTagsForDomain_domainName :: Lens.Lens' ListTagsForDomain Prelude.Text
listTagsForDomain_domainName = Lens.lens (\ListTagsForDomain' {domainName} -> domainName) (\s@ListTagsForDomain' {} a -> s {domainName = a} :: ListTagsForDomain)

instance Core.AWSRequest ListTagsForDomain where
  type
    AWSResponse ListTagsForDomain =
      ListTagsForDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForDomainResponse'
            Prelude.<$> (x Data..?> "TagList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForDomain where
  hashWithSalt _salt ListTagsForDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListTagsForDomain where
  rnf ListTagsForDomain' {..} = Prelude.rnf domainName

instance Data.ToHeaders ListTagsForDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ListTagsForDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForDomain where
  toJSON ListTagsForDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath ListTagsForDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForDomain where
  toQuery = Prelude.const Prelude.mempty

-- | The ListTagsForDomain response includes the following elements.
--
-- /See:/ 'newListTagsForDomainResponse' smart constructor.
data ListTagsForDomainResponse = ListTagsForDomainResponse'
  { -- | A list of the tags that are associated with the specified domain.
    tagList :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'listTagsForDomainResponse_tagList' - A list of the tags that are associated with the specified domain.
--
-- 'httpStatus', 'listTagsForDomainResponse_httpStatus' - The response's http status code.
newListTagsForDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForDomainResponse
newListTagsForDomainResponse pHttpStatus_ =
  ListTagsForDomainResponse'
    { tagList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the tags that are associated with the specified domain.
listTagsForDomainResponse_tagList :: Lens.Lens' ListTagsForDomainResponse (Prelude.Maybe [Tag])
listTagsForDomainResponse_tagList = Lens.lens (\ListTagsForDomainResponse' {tagList} -> tagList) (\s@ListTagsForDomainResponse' {} a -> s {tagList = a} :: ListTagsForDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForDomainResponse_httpStatus :: Lens.Lens' ListTagsForDomainResponse Prelude.Int
listTagsForDomainResponse_httpStatus = Lens.lens (\ListTagsForDomainResponse' {httpStatus} -> httpStatus) (\s@ListTagsForDomainResponse' {} a -> s {httpStatus = a} :: ListTagsForDomainResponse)

instance Prelude.NFData ListTagsForDomainResponse where
  rnf ListTagsForDomainResponse' {..} =
    Prelude.rnf tagList `Prelude.seq`
      Prelude.rnf httpStatus
