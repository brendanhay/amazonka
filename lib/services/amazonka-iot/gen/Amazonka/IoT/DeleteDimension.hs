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
-- Module      : Amazonka.IoT.DeleteDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified dimension from your Amazon Web Services accounts.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteDimension>
-- action.
module Amazonka.IoT.DeleteDimension
  ( -- * Creating a Request
    DeleteDimension (..),
    newDeleteDimension,

    -- * Request Lenses
    deleteDimension_name,

    -- * Destructuring the Response
    DeleteDimensionResponse (..),
    newDeleteDimensionResponse,

    -- * Response Lenses
    deleteDimensionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDimension' smart constructor.
data DeleteDimension = DeleteDimension'
  { -- | The unique identifier for the dimension that you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteDimension_name' - The unique identifier for the dimension that you want to delete.
newDeleteDimension ::
  -- | 'name'
  Prelude.Text ->
  DeleteDimension
newDeleteDimension pName_ =
  DeleteDimension' {name = pName_}

-- | The unique identifier for the dimension that you want to delete.
deleteDimension_name :: Lens.Lens' DeleteDimension Prelude.Text
deleteDimension_name = Lens.lens (\DeleteDimension' {name} -> name) (\s@DeleteDimension' {} a -> s {name = a} :: DeleteDimension)

instance Core.AWSRequest DeleteDimension where
  type
    AWSResponse DeleteDimension =
      DeleteDimensionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDimensionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDimension where
  hashWithSalt _salt DeleteDimension' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDimension where
  rnf DeleteDimension' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDimension where
  toPath DeleteDimension' {..} =
    Prelude.mconcat ["/dimensions/", Data.toBS name]

instance Data.ToQuery DeleteDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDimensionResponse' smart constructor.
data DeleteDimensionResponse = DeleteDimensionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDimensionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDimensionResponse_httpStatus' - The response's http status code.
newDeleteDimensionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDimensionResponse
newDeleteDimensionResponse pHttpStatus_ =
  DeleteDimensionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDimensionResponse_httpStatus :: Lens.Lens' DeleteDimensionResponse Prelude.Int
deleteDimensionResponse_httpStatus = Lens.lens (\DeleteDimensionResponse' {httpStatus} -> httpStatus) (\s@DeleteDimensionResponse' {} a -> s {httpStatus = a} :: DeleteDimensionResponse)

instance Prelude.NFData DeleteDimensionResponse where
  rnf DeleteDimensionResponse' {..} =
    Prelude.rnf httpStatus
