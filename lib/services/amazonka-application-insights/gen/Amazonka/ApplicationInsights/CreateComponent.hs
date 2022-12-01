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
-- Module      : Amazonka.ApplicationInsights.CreateComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom component by grouping similar standalone instances to
-- monitor.
module Amazonka.ApplicationInsights.CreateComponent
  ( -- * Creating a Request
    CreateComponent (..),
    newCreateComponent,

    -- * Request Lenses
    createComponent_resourceGroupName,
    createComponent_componentName,
    createComponent_resourceList,

    -- * Destructuring the Response
    CreateComponentResponse (..),
    newCreateComponentResponse,

    -- * Response Lenses
    createComponentResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateComponent' smart constructor.
data CreateComponent = CreateComponent'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Text,
    -- | The list of resource ARNs that belong to the component.
    resourceList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'createComponent_resourceGroupName' - The name of the resource group.
--
-- 'componentName', 'createComponent_componentName' - The name of the component.
--
-- 'resourceList', 'createComponent_resourceList' - The list of resource ARNs that belong to the component.
newCreateComponent ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'componentName'
  Prelude.Text ->
  CreateComponent
newCreateComponent
  pResourceGroupName_
  pComponentName_ =
    CreateComponent'
      { resourceGroupName =
          pResourceGroupName_,
        componentName = pComponentName_,
        resourceList = Prelude.mempty
      }

-- | The name of the resource group.
createComponent_resourceGroupName :: Lens.Lens' CreateComponent Prelude.Text
createComponent_resourceGroupName = Lens.lens (\CreateComponent' {resourceGroupName} -> resourceGroupName) (\s@CreateComponent' {} a -> s {resourceGroupName = a} :: CreateComponent)

-- | The name of the component.
createComponent_componentName :: Lens.Lens' CreateComponent Prelude.Text
createComponent_componentName = Lens.lens (\CreateComponent' {componentName} -> componentName) (\s@CreateComponent' {} a -> s {componentName = a} :: CreateComponent)

-- | The list of resource ARNs that belong to the component.
createComponent_resourceList :: Lens.Lens' CreateComponent [Prelude.Text]
createComponent_resourceList = Lens.lens (\CreateComponent' {resourceList} -> resourceList) (\s@CreateComponent' {} a -> s {resourceList = a} :: CreateComponent) Prelude.. Lens.coerced

instance Core.AWSRequest CreateComponent where
  type
    AWSResponse CreateComponent =
      CreateComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateComponentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComponent where
  hashWithSalt _salt CreateComponent' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` resourceList

instance Prelude.NFData CreateComponent where
  rnf CreateComponent' {..} =
    Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf resourceList

instance Core.ToHeaders CreateComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "EC2WindowsBarleyService.CreateComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateComponent where
  toJSON CreateComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Core..= resourceGroupName),
            Prelude.Just ("ComponentName" Core..= componentName),
            Prelude.Just ("ResourceList" Core..= resourceList)
          ]
      )

instance Core.ToPath CreateComponent where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateComponentResponse' smart constructor.
data CreateComponentResponse = CreateComponentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createComponentResponse_httpStatus' - The response's http status code.
newCreateComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComponentResponse
newCreateComponentResponse pHttpStatus_ =
  CreateComponentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createComponentResponse_httpStatus :: Lens.Lens' CreateComponentResponse Prelude.Int
createComponentResponse_httpStatus = Lens.lens (\CreateComponentResponse' {httpStatus} -> httpStatus) (\s@CreateComponentResponse' {} a -> s {httpStatus = a} :: CreateComponentResponse)

instance Prelude.NFData CreateComponentResponse where
  rnf CreateComponentResponse' {..} =
    Prelude.rnf httpStatus
