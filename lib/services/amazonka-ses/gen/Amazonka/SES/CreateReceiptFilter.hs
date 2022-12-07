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
-- Module      : Amazonka.SES.CreateReceiptFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IP address filter.
--
-- For information about setting up IP address filters, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.CreateReceiptFilter
  ( -- * Creating a Request
    CreateReceiptFilter (..),
    newCreateReceiptFilter,

    -- * Request Lenses
    createReceiptFilter_filter,

    -- * Destructuring the Response
    CreateReceiptFilterResponse (..),
    newCreateReceiptFilterResponse,

    -- * Response Lenses
    createReceiptFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to create a new IP address filter. You use IP
-- address filters when you receive email with Amazon SES. For more
-- information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateReceiptFilter' smart constructor.
data CreateReceiptFilter = CreateReceiptFilter'
  { -- | A data structure that describes the IP address filter to create, which
    -- consists of a name, an IP address range, and whether to allow or block
    -- mail from it.
    filter' :: ReceiptFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReceiptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'createReceiptFilter_filter' - A data structure that describes the IP address filter to create, which
-- consists of a name, an IP address range, and whether to allow or block
-- mail from it.
newCreateReceiptFilter ::
  -- | 'filter''
  ReceiptFilter ->
  CreateReceiptFilter
newCreateReceiptFilter pFilter_ =
  CreateReceiptFilter' {filter' = pFilter_}

-- | A data structure that describes the IP address filter to create, which
-- consists of a name, an IP address range, and whether to allow or block
-- mail from it.
createReceiptFilter_filter :: Lens.Lens' CreateReceiptFilter ReceiptFilter
createReceiptFilter_filter = Lens.lens (\CreateReceiptFilter' {filter'} -> filter') (\s@CreateReceiptFilter' {} a -> s {filter' = a} :: CreateReceiptFilter)

instance Core.AWSRequest CreateReceiptFilter where
  type
    AWSResponse CreateReceiptFilter =
      CreateReceiptFilterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateReceiptFilterResult"
      ( \s h x ->
          CreateReceiptFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReceiptFilter where
  hashWithSalt _salt CreateReceiptFilter' {..} =
    _salt `Prelude.hashWithSalt` filter'

instance Prelude.NFData CreateReceiptFilter where
  rnf CreateReceiptFilter' {..} = Prelude.rnf filter'

instance Data.ToHeaders CreateReceiptFilter where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateReceiptFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReceiptFilter where
  toQuery CreateReceiptFilter' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateReceiptFilter" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Filter" Data.=: filter'
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateReceiptFilterResponse' smart constructor.
data CreateReceiptFilterResponse = CreateReceiptFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReceiptFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createReceiptFilterResponse_httpStatus' - The response's http status code.
newCreateReceiptFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReceiptFilterResponse
newCreateReceiptFilterResponse pHttpStatus_ =
  CreateReceiptFilterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createReceiptFilterResponse_httpStatus :: Lens.Lens' CreateReceiptFilterResponse Prelude.Int
createReceiptFilterResponse_httpStatus = Lens.lens (\CreateReceiptFilterResponse' {httpStatus} -> httpStatus) (\s@CreateReceiptFilterResponse' {} a -> s {httpStatus = a} :: CreateReceiptFilterResponse)

instance Prelude.NFData CreateReceiptFilterResponse where
  rnf CreateReceiptFilterResponse' {..} =
    Prelude.rnf httpStatus
