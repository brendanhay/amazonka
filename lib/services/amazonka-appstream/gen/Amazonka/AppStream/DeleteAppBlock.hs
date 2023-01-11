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
-- Module      : Amazonka.AppStream.DeleteAppBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an app block.
module Amazonka.AppStream.DeleteAppBlock
  ( -- * Creating a Request
    DeleteAppBlock (..),
    newDeleteAppBlock,

    -- * Request Lenses
    deleteAppBlock_name,

    -- * Destructuring the Response
    DeleteAppBlockResponse (..),
    newDeleteAppBlockResponse,

    -- * Response Lenses
    deleteAppBlockResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppBlock' smart constructor.
data DeleteAppBlock = DeleteAppBlock'
  { -- | The name of the app block.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteAppBlock_name' - The name of the app block.
newDeleteAppBlock ::
  -- | 'name'
  Prelude.Text ->
  DeleteAppBlock
newDeleteAppBlock pName_ =
  DeleteAppBlock' {name = pName_}

-- | The name of the app block.
deleteAppBlock_name :: Lens.Lens' DeleteAppBlock Prelude.Text
deleteAppBlock_name = Lens.lens (\DeleteAppBlock' {name} -> name) (\s@DeleteAppBlock' {} a -> s {name = a} :: DeleteAppBlock)

instance Core.AWSRequest DeleteAppBlock where
  type
    AWSResponse DeleteAppBlock =
      DeleteAppBlockResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppBlockResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAppBlock where
  hashWithSalt _salt DeleteAppBlock' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteAppBlock where
  rnf DeleteAppBlock' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteAppBlock where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteAppBlock" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAppBlock where
  toJSON DeleteAppBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteAppBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAppBlock where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppBlockResponse' smart constructor.
data DeleteAppBlockResponse = DeleteAppBlockResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppBlockResponse_httpStatus' - The response's http status code.
newDeleteAppBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppBlockResponse
newDeleteAppBlockResponse pHttpStatus_ =
  DeleteAppBlockResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAppBlockResponse_httpStatus :: Lens.Lens' DeleteAppBlockResponse Prelude.Int
deleteAppBlockResponse_httpStatus = Lens.lens (\DeleteAppBlockResponse' {httpStatus} -> httpStatus) (\s@DeleteAppBlockResponse' {} a -> s {httpStatus = a} :: DeleteAppBlockResponse)

instance Prelude.NFData DeleteAppBlockResponse where
  rnf DeleteAppBlockResponse' {..} =
    Prelude.rnf httpStatus
