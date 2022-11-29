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
-- Module      : Amazonka.Location.DeletePlaceIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a place index resource from your AWS account.
--
-- This operation deletes the resource permanently.
module Amazonka.Location.DeletePlaceIndex
  ( -- * Creating a Request
    DeletePlaceIndex (..),
    newDeletePlaceIndex,

    -- * Request Lenses
    deletePlaceIndex_indexName,

    -- * Destructuring the Response
    DeletePlaceIndexResponse (..),
    newDeletePlaceIndexResponse,

    -- * Response Lenses
    deletePlaceIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePlaceIndex' smart constructor.
data DeletePlaceIndex = DeletePlaceIndex'
  { -- | The name of the place index resource to be deleted.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlaceIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'deletePlaceIndex_indexName' - The name of the place index resource to be deleted.
newDeletePlaceIndex ::
  -- | 'indexName'
  Prelude.Text ->
  DeletePlaceIndex
newDeletePlaceIndex pIndexName_ =
  DeletePlaceIndex' {indexName = pIndexName_}

-- | The name of the place index resource to be deleted.
deletePlaceIndex_indexName :: Lens.Lens' DeletePlaceIndex Prelude.Text
deletePlaceIndex_indexName = Lens.lens (\DeletePlaceIndex' {indexName} -> indexName) (\s@DeletePlaceIndex' {} a -> s {indexName = a} :: DeletePlaceIndex)

instance Core.AWSRequest DeletePlaceIndex where
  type
    AWSResponse DeletePlaceIndex =
      DeletePlaceIndexResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePlaceIndexResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePlaceIndex where
  hashWithSalt _salt DeletePlaceIndex' {..} =
    _salt `Prelude.hashWithSalt` indexName

instance Prelude.NFData DeletePlaceIndex where
  rnf DeletePlaceIndex' {..} = Prelude.rnf indexName

instance Core.ToHeaders DeletePlaceIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeletePlaceIndex where
  toPath DeletePlaceIndex' {..} =
    Prelude.mconcat
      ["/places/v0/indexes/", Core.toBS indexName]

instance Core.ToQuery DeletePlaceIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePlaceIndexResponse' smart constructor.
data DeletePlaceIndexResponse = DeletePlaceIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePlaceIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePlaceIndexResponse_httpStatus' - The response's http status code.
newDeletePlaceIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePlaceIndexResponse
newDeletePlaceIndexResponse pHttpStatus_ =
  DeletePlaceIndexResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePlaceIndexResponse_httpStatus :: Lens.Lens' DeletePlaceIndexResponse Prelude.Int
deletePlaceIndexResponse_httpStatus = Lens.lens (\DeletePlaceIndexResponse' {httpStatus} -> httpStatus) (\s@DeletePlaceIndexResponse' {} a -> s {httpStatus = a} :: DeletePlaceIndexResponse)

instance Prelude.NFData DeletePlaceIndexResponse where
  rnf DeletePlaceIndexResponse' {..} =
    Prelude.rnf httpStatus
