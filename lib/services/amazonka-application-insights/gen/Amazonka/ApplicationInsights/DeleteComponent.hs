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
-- Module      : Amazonka.ApplicationInsights.DeleteComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ungroups a custom component. When you ungroup custom components, all
-- applicable monitors that are set up for the component are removed and
-- the instances revert to their standalone status.
module Amazonka.ApplicationInsights.DeleteComponent
  ( -- * Creating a Request
    DeleteComponent (..),
    newDeleteComponent,

    -- * Request Lenses
    deleteComponent_resourceGroupName,
    deleteComponent_componentName,

    -- * Destructuring the Response
    DeleteComponentResponse (..),
    newDeleteComponentResponse,

    -- * Response Lenses
    deleteComponentResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponent' smart constructor.
data DeleteComponent = DeleteComponent'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Text
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
-- 'resourceGroupName', 'deleteComponent_resourceGroupName' - The name of the resource group.
--
-- 'componentName', 'deleteComponent_componentName' - The name of the component.
newDeleteComponent ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'componentName'
  Prelude.Text ->
  DeleteComponent
newDeleteComponent
  pResourceGroupName_
  pComponentName_ =
    DeleteComponent'
      { resourceGroupName =
          pResourceGroupName_,
        componentName = pComponentName_
      }

-- | The name of the resource group.
deleteComponent_resourceGroupName :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_resourceGroupName = Lens.lens (\DeleteComponent' {resourceGroupName} -> resourceGroupName) (\s@DeleteComponent' {} a -> s {resourceGroupName = a} :: DeleteComponent)

-- | The name of the component.
deleteComponent_componentName :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_componentName = Lens.lens (\DeleteComponent' {componentName} -> componentName) (\s@DeleteComponent' {} a -> s {componentName = a} :: DeleteComponent)

instance Core.AWSRequest DeleteComponent where
  type
    AWSResponse DeleteComponent =
      DeleteComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteComponentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteComponent where
  hashWithSalt _salt DeleteComponent' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` componentName

instance Prelude.NFData DeleteComponent where
  rnf DeleteComponent' {..} =
    Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf componentName

instance Data.ToHeaders DeleteComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DeleteComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteComponent where
  toJSON DeleteComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("ComponentName" Data..= componentName)
          ]
      )

instance Data.ToPath DeleteComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteComponentResponse' smart constructor.
data DeleteComponentResponse = DeleteComponentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteComponentResponse_httpStatus' - The response's http status code.
newDeleteComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteComponentResponse
newDeleteComponentResponse pHttpStatus_ =
  DeleteComponentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteComponentResponse_httpStatus :: Lens.Lens' DeleteComponentResponse Prelude.Int
deleteComponentResponse_httpStatus = Lens.lens (\DeleteComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteComponentResponse' {} a -> s {httpStatus = a} :: DeleteComponentResponse)

instance Prelude.NFData DeleteComponentResponse where
  rnf DeleteComponentResponse' {..} =
    Prelude.rnf httpStatus
