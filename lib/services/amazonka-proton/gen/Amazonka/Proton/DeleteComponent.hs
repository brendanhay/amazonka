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
-- Module      : Amazonka.Proton.DeleteComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an Proton component resource.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.DeleteComponent
  ( -- * Creating a Request
    DeleteComponent (..),
    newDeleteComponent,

    -- * Request Lenses
    deleteComponent_name,

    -- * Destructuring the Response
    DeleteComponentResponse (..),
    newDeleteComponentResponse,

    -- * Response Lenses
    deleteComponentResponse_component,
    deleteComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponent' smart constructor.
data DeleteComponent = DeleteComponent'
  { -- | The name of the component to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteComponent_name' - The name of the component to delete.
newDeleteComponent ::
  -- | 'name'
  Prelude.Text ->
  DeleteComponent
newDeleteComponent pName_ =
  DeleteComponent' {name = pName_}

-- | The name of the component to delete.
deleteComponent_name :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_name = Lens.lens (\DeleteComponent' {name} -> name) (\s@DeleteComponent' {} a -> s {name = a} :: DeleteComponent)

instance Core.AWSRequest DeleteComponent where
  type
    AWSResponse DeleteComponent =
      DeleteComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteComponentResponse'
            Prelude.<$> (x Data..?> "component")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteComponent where
  hashWithSalt _salt DeleteComponent' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteComponent where
  rnf DeleteComponent' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.DeleteComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteComponent where
  toJSON DeleteComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComponentResponse' smart constructor.
data DeleteComponentResponse = DeleteComponentResponse'
  { -- | The detailed data of the component being deleted.
    component :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'component', 'deleteComponentResponse_component' - The detailed data of the component being deleted.
--
-- 'httpStatus', 'deleteComponentResponse_httpStatus' - The response's http status code.
newDeleteComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteComponentResponse
newDeleteComponentResponse pHttpStatus_ =
  DeleteComponentResponse'
    { component =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the component being deleted.
deleteComponentResponse_component :: Lens.Lens' DeleteComponentResponse (Prelude.Maybe Component)
deleteComponentResponse_component = Lens.lens (\DeleteComponentResponse' {component} -> component) (\s@DeleteComponentResponse' {} a -> s {component = a} :: DeleteComponentResponse)

-- | The response's http status code.
deleteComponentResponse_httpStatus :: Lens.Lens' DeleteComponentResponse Prelude.Int
deleteComponentResponse_httpStatus = Lens.lens (\DeleteComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteComponentResponse' {} a -> s {httpStatus = a} :: DeleteComponentResponse)

instance Prelude.NFData DeleteComponentResponse where
  rnf DeleteComponentResponse' {..} =
    Prelude.rnf component `Prelude.seq`
      Prelude.rnf httpStatus
