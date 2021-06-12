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
-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all search domains owned by an account.
module Network.AWS.CloudSearch.ListDomainNames
  ( -- * Creating a Request
    ListDomainNames (..),
    newListDomainNames,

    -- * Destructuring the Response
    ListDomainNamesResponse (..),
    newListDomainNamesResponse,

    -- * Response Lenses
    listDomainNamesResponse_domainNames,
    listDomainNamesResponse_httpStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListDomainNames ::
  ListDomainNames
newListDomainNames = ListDomainNames'

instance Core.AWSRequest ListDomainNames where
  type
    AWSResponse ListDomainNames =
      ListDomainNamesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListDomainNamesResult"
      ( \s h x ->
          ListDomainNamesResponse'
            Core.<$> ( x Core..@? "DomainNames" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDomainNames

instance Core.NFData ListDomainNames

instance Core.ToHeaders ListDomainNames where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListDomainNames where
  toPath = Core.const "/"

instance Core.ToQuery ListDomainNames where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("ListDomainNames" :: Core.ByteString),
            "Version" Core.=: ("2013-01-01" :: Core.ByteString)
          ]
      )

-- | The result of a @ListDomainNames@ request. Contains a list of the
-- domains owned by an account.
--
-- /See:/ 'newListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { -- | The names of the search domains owned by an account.
    domainNames :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainNames', 'listDomainNamesResponse_domainNames' - The names of the search domains owned by an account.
--
-- 'httpStatus', 'listDomainNamesResponse_httpStatus' - The response's http status code.
newListDomainNamesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDomainNamesResponse
newListDomainNamesResponse pHttpStatus_ =
  ListDomainNamesResponse'
    { domainNames =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the search domains owned by an account.
listDomainNamesResponse_domainNames :: Lens.Lens' ListDomainNamesResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listDomainNamesResponse_domainNames = Lens.lens (\ListDomainNamesResponse' {domainNames} -> domainNames) (\s@ListDomainNamesResponse' {} a -> s {domainNames = a} :: ListDomainNamesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDomainNamesResponse_httpStatus :: Lens.Lens' ListDomainNamesResponse Core.Int
listDomainNamesResponse_httpStatus = Lens.lens (\ListDomainNamesResponse' {httpStatus} -> httpStatus) (\s@ListDomainNamesResponse' {} a -> s {httpStatus = a} :: ListDomainNamesResponse)

instance Core.NFData ListDomainNamesResponse
