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
-- Module      : Amazonka.SES.ListReceiptFilters
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
module Amazonka.SES.ListReceiptFilters
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to list the IP address filters that exist under
-- your AWS account. You use IP address filters when you receive email with
-- Amazon SES. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newListReceiptFilters' smart constructor.
data ListReceiptFilters = ListReceiptFilters'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
            Prelude.<$> ( x Core..@? "Filters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReceiptFilters where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListReceiptFilters where
  rnf _ = ()

instance Core.ToHeaders ListReceiptFilters where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListReceiptFilters where
  toPath = Prelude.const "/"

instance Core.ToQuery ListReceiptFilters where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Core.=: ("ListReceiptFilters" :: Prelude.ByteString),
            "Version"
              Core.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | A list of IP address filters that exist under your AWS account.
--
-- /See:/ 'newListReceiptFiltersResponse' smart constructor.
data ListReceiptFiltersResponse = ListReceiptFiltersResponse'
  { -- | A list of IP address filter data structures, which each consist of a
    -- name, an IP address range, and whether to allow or block mail from it.
    filters :: Prelude.Maybe [ReceiptFilter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListReceiptFiltersResponse
newListReceiptFiltersResponse pHttpStatus_ =
  ListReceiptFiltersResponse'
    { filters =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of IP address filter data structures, which each consist of a
-- name, an IP address range, and whether to allow or block mail from it.
listReceiptFiltersResponse_filters :: Lens.Lens' ListReceiptFiltersResponse (Prelude.Maybe [ReceiptFilter])
listReceiptFiltersResponse_filters = Lens.lens (\ListReceiptFiltersResponse' {filters} -> filters) (\s@ListReceiptFiltersResponse' {} a -> s {filters = a} :: ListReceiptFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReceiptFiltersResponse_httpStatus :: Lens.Lens' ListReceiptFiltersResponse Prelude.Int
listReceiptFiltersResponse_httpStatus = Lens.lens (\ListReceiptFiltersResponse' {httpStatus} -> httpStatus) (\s@ListReceiptFiltersResponse' {} a -> s {httpStatus = a} :: ListReceiptFiltersResponse)

instance Prelude.NFData ListReceiptFiltersResponse where
  rnf ListReceiptFiltersResponse' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf httpStatus
