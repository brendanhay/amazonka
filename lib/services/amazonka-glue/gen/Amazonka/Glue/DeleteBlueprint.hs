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
-- Module      : Amazonka.Glue.DeleteBlueprint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing blueprint.
module Amazonka.Glue.DeleteBlueprint
  ( -- * Creating a Request
    DeleteBlueprint (..),
    newDeleteBlueprint,

    -- * Request Lenses
    deleteBlueprint_name,

    -- * Destructuring the Response
    DeleteBlueprintResponse (..),
    newDeleteBlueprintResponse,

    -- * Response Lenses
    deleteBlueprintResponse_name,
    deleteBlueprintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBlueprint' smart constructor.
data DeleteBlueprint = DeleteBlueprint'
  { -- | The name of the blueprint to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBlueprint_name' - The name of the blueprint to delete.
newDeleteBlueprint ::
  -- | 'name'
  Prelude.Text ->
  DeleteBlueprint
newDeleteBlueprint pName_ =
  DeleteBlueprint' {name = pName_}

-- | The name of the blueprint to delete.
deleteBlueprint_name :: Lens.Lens' DeleteBlueprint Prelude.Text
deleteBlueprint_name = Lens.lens (\DeleteBlueprint' {name} -> name) (\s@DeleteBlueprint' {} a -> s {name = a} :: DeleteBlueprint)

instance Core.AWSRequest DeleteBlueprint where
  type
    AWSResponse DeleteBlueprint =
      DeleteBlueprintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBlueprintResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBlueprint where
  hashWithSalt _salt DeleteBlueprint' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteBlueprint where
  rnf DeleteBlueprint' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteBlueprint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteBlueprint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteBlueprint where
  toJSON DeleteBlueprint' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DeleteBlueprint where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteBlueprint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBlueprintResponse' smart constructor.
data DeleteBlueprintResponse = DeleteBlueprintResponse'
  { -- | Returns the name of the blueprint that was deleted.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBlueprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBlueprintResponse_name' - Returns the name of the blueprint that was deleted.
--
-- 'httpStatus', 'deleteBlueprintResponse_httpStatus' - The response's http status code.
newDeleteBlueprintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBlueprintResponse
newDeleteBlueprintResponse pHttpStatus_ =
  DeleteBlueprintResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the name of the blueprint that was deleted.
deleteBlueprintResponse_name :: Lens.Lens' DeleteBlueprintResponse (Prelude.Maybe Prelude.Text)
deleteBlueprintResponse_name = Lens.lens (\DeleteBlueprintResponse' {name} -> name) (\s@DeleteBlueprintResponse' {} a -> s {name = a} :: DeleteBlueprintResponse)

-- | The response's http status code.
deleteBlueprintResponse_httpStatus :: Lens.Lens' DeleteBlueprintResponse Prelude.Int
deleteBlueprintResponse_httpStatus = Lens.lens (\DeleteBlueprintResponse' {httpStatus} -> httpStatus) (\s@DeleteBlueprintResponse' {} a -> s {httpStatus = a} :: DeleteBlueprintResponse)

instance Prelude.NFData DeleteBlueprintResponse where
  rnf DeleteBlueprintResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
