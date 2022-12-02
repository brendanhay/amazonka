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
-- Module      : Amazonka.SSMSAP.GetComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the component of an application registered with AWS Systems Manager
-- for SAP.
module Amazonka.SSMSAP.GetComponent
  ( -- * Creating a Request
    GetComponent (..),
    newGetComponent,

    -- * Request Lenses
    getComponent_applicationId,
    getComponent_componentId,

    -- * Destructuring the Response
    GetComponentResponse (..),
    newGetComponentResponse,

    -- * Response Lenses
    getComponentResponse_component,
    getComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetComponent' smart constructor.
data GetComponent = GetComponent'
  { applicationId :: Prelude.Text,
    componentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getComponent_applicationId' -
--
-- 'componentId', 'getComponent_componentId' -
newGetComponent ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'componentId'
  Prelude.Text ->
  GetComponent
newGetComponent pApplicationId_ pComponentId_ =
  GetComponent'
    { applicationId = pApplicationId_,
      componentId = pComponentId_
    }

-- |
getComponent_applicationId :: Lens.Lens' GetComponent Prelude.Text
getComponent_applicationId = Lens.lens (\GetComponent' {applicationId} -> applicationId) (\s@GetComponent' {} a -> s {applicationId = a} :: GetComponent)

-- |
getComponent_componentId :: Lens.Lens' GetComponent Prelude.Text
getComponent_componentId = Lens.lens (\GetComponent' {componentId} -> componentId) (\s@GetComponent' {} a -> s {componentId = a} :: GetComponent)

instance Core.AWSRequest GetComponent where
  type AWSResponse GetComponent = GetComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentResponse'
            Prelude.<$> (x Data..?> "Component")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponent where
  hashWithSalt _salt GetComponent' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` componentId

instance Prelude.NFData GetComponent where
  rnf GetComponent' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf componentId

instance Data.ToHeaders GetComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComponent where
  toJSON GetComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationId" Data..= applicationId),
            Prelude.Just ("ComponentId" Data..= componentId)
          ]
      )

instance Data.ToPath GetComponent where
  toPath = Prelude.const "/get-component"

instance Data.ToQuery GetComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComponentResponse' smart constructor.
data GetComponentResponse = GetComponentResponse'
  { component :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'component', 'getComponentResponse_component' -
--
-- 'httpStatus', 'getComponentResponse_httpStatus' - The response's http status code.
newGetComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComponentResponse
newGetComponentResponse pHttpStatus_ =
  GetComponentResponse'
    { component = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
getComponentResponse_component :: Lens.Lens' GetComponentResponse (Prelude.Maybe Component)
getComponentResponse_component = Lens.lens (\GetComponentResponse' {component} -> component) (\s@GetComponentResponse' {} a -> s {component = a} :: GetComponentResponse)

-- | The response's http status code.
getComponentResponse_httpStatus :: Lens.Lens' GetComponentResponse Prelude.Int
getComponentResponse_httpStatus = Lens.lens (\GetComponentResponse' {httpStatus} -> httpStatus) (\s@GetComponentResponse' {} a -> s {httpStatus = a} :: GetComponentResponse)

instance Prelude.NFData GetComponentResponse where
  rnf GetComponentResponse' {..} =
    Prelude.rnf component
      `Prelude.seq` Prelude.rnf httpStatus
