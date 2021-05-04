{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DeleteDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified dimension from your AWS account.
module Network.AWS.IoT.DeleteDimension
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDimension' smart constructor.
data DeleteDimension = DeleteDimension'
  { -- | The unique identifier for the dimension that you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteDimension where
  type Rs DeleteDimension = DeleteDimensionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDimensionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDimension

instance Prelude.NFData DeleteDimension

instance Prelude.ToHeaders DeleteDimension where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDimension where
  toPath DeleteDimension' {..} =
    Prelude.mconcat ["/dimensions/", Prelude.toBS name]

instance Prelude.ToQuery DeleteDimension where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDimensionResponse' smart constructor.
data DeleteDimensionResponse = DeleteDimensionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteDimensionResponse
