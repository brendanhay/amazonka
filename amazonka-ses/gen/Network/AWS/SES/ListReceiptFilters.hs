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
-- Module      : Network.AWS.SES.ListReceiptFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IP address filters associated with your AWS account in the
-- current AWS Region.
--
-- For information about managing IP address filters, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ListReceiptFilters
  ( -- * Creating a Request
    ListReceiptFilters (..),
    newListReceiptFilters,

    -- * Destructuring the Response
    ListReceiptFiltersResponse (..),
    newListReceiptFiltersResponse,

    -- * Response Lenses
    listReceiptFiltersResponse_filters,
    listReceiptFiltersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to list the IP address filters that exist under
-- your AWS account. You use IP address filters when you receive email with
-- Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListReceiptFilters' smart constructor.
data ListReceiptFilters = ListReceiptFilters'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReceiptFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListReceiptFilters ::
  ListReceiptFilters
newListReceiptFilters = ListReceiptFilters'

instance Core.AWSRequest ListReceiptFilters where
  type
    AWSResponse ListReceiptFilters =
      ListReceiptFiltersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListReceiptFiltersResult"
      ( \s h x ->
          ListReceiptFiltersResponse'
            Core.<$> ( x Core..@? "Filters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReceiptFilters

instance Core.NFData ListReceiptFilters

instance Core.ToHeaders ListReceiptFilters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListReceiptFilters where
  toPath = Core.const "/"

instance Core.ToQuery ListReceiptFilters where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("ListReceiptFilters" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | A list of IP address filters that exist under your AWS account.
--
-- /See:/ 'newListReceiptFiltersResponse' smart constructor.
data ListReceiptFiltersResponse = ListReceiptFiltersResponse'
  { -- | A list of IP address filter data structures, which each consist of a
    -- name, an IP address range, and whether to allow or block mail from it.
    filters :: Core.Maybe [ReceiptFilter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReceiptFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listReceiptFiltersResponse_filters' - A list of IP address filter data structures, which each consist of a
-- name, an IP address range, and whether to allow or block mail from it.
--
-- 'httpStatus', 'listReceiptFiltersResponse_httpStatus' - The response's http status code.
newListReceiptFiltersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReceiptFiltersResponse
newListReceiptFiltersResponse pHttpStatus_ =
  ListReceiptFiltersResponse'
    { filters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of IP address filter data structures, which each consist of a
-- name, an IP address range, and whether to allow or block mail from it.
listReceiptFiltersResponse_filters :: Lens.Lens' ListReceiptFiltersResponse (Core.Maybe [ReceiptFilter])
listReceiptFiltersResponse_filters = Lens.lens (\ListReceiptFiltersResponse' {filters} -> filters) (\s@ListReceiptFiltersResponse' {} a -> s {filters = a} :: ListReceiptFiltersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReceiptFiltersResponse_httpStatus :: Lens.Lens' ListReceiptFiltersResponse Core.Int
listReceiptFiltersResponse_httpStatus = Lens.lens (\ListReceiptFiltersResponse' {httpStatus} -> httpStatus) (\s@ListReceiptFiltersResponse' {} a -> s {httpStatus = a} :: ListReceiptFiltersResponse)

instance Core.NFData ListReceiptFiltersResponse
