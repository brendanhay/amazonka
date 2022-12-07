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
-- Module      : Amazonka.SES.DeleteReceiptFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP address filter.
--
-- For information about managing IP address filters, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteReceiptFilter
  ( -- * Creating a Request
    DeleteReceiptFilter (..),
    newDeleteReceiptFilter,

    -- * Request Lenses
    deleteReceiptFilter_filterName,

    -- * Destructuring the Response
    DeleteReceiptFilterResponse (..),
    newDeleteReceiptFilterResponse,

    -- * Response Lenses
    deleteReceiptFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete an IP address filter. You use IP address
-- filters when you receive email with Amazon SES. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteReceiptFilter' smart constructor.
data DeleteReceiptFilter = DeleteReceiptFilter'
  { -- | The name of the IP address filter to delete.
    filterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterName', 'deleteReceiptFilter_filterName' - The name of the IP address filter to delete.
newDeleteReceiptFilter ::
  -- | 'filterName'
  Prelude.Text ->
  DeleteReceiptFilter
newDeleteReceiptFilter pFilterName_ =
  DeleteReceiptFilter' {filterName = pFilterName_}

-- | The name of the IP address filter to delete.
deleteReceiptFilter_filterName :: Lens.Lens' DeleteReceiptFilter Prelude.Text
deleteReceiptFilter_filterName = Lens.lens (\DeleteReceiptFilter' {filterName} -> filterName) (\s@DeleteReceiptFilter' {} a -> s {filterName = a} :: DeleteReceiptFilter)

instance Core.AWSRequest DeleteReceiptFilter where
  type
    AWSResponse DeleteReceiptFilter =
      DeleteReceiptFilterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteReceiptFilterResult"
      ( \s h x ->
          DeleteReceiptFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReceiptFilter where
  hashWithSalt _salt DeleteReceiptFilter' {..} =
    _salt `Prelude.hashWithSalt` filterName

instance Prelude.NFData DeleteReceiptFilter where
  rnf DeleteReceiptFilter' {..} = Prelude.rnf filterName

instance Data.ToHeaders DeleteReceiptFilter where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteReceiptFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteReceiptFilter where
  toQuery DeleteReceiptFilter' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteReceiptFilter" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "FilterName" Data.=: filterName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteReceiptFilterResponse' smart constructor.
data DeleteReceiptFilterResponse = DeleteReceiptFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReceiptFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReceiptFilterResponse_httpStatus' - The response's http status code.
newDeleteReceiptFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReceiptFilterResponse
newDeleteReceiptFilterResponse pHttpStatus_ =
  DeleteReceiptFilterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReceiptFilterResponse_httpStatus :: Lens.Lens' DeleteReceiptFilterResponse Prelude.Int
deleteReceiptFilterResponse_httpStatus = Lens.lens (\DeleteReceiptFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteReceiptFilterResponse' {} a -> s {httpStatus = a} :: DeleteReceiptFilterResponse)

instance Prelude.NFData DeleteReceiptFilterResponse where
  rnf DeleteReceiptFilterResponse' {..} =
    Prelude.rnf httpStatus
