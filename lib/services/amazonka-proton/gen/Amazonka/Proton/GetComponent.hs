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
-- Module      : Amazonka.Proton.GetComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed data for a component.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
module Amazonka.Proton.GetComponent
  ( -- * Creating a Request
    GetComponent (..),
    newGetComponent,

    -- * Request Lenses
    getComponent_name,

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
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComponent' smart constructor.
data GetComponent = GetComponent'
  { -- | The name of the component that you want to get the detailed data for.
    name :: Prelude.Text
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
-- 'name', 'getComponent_name' - The name of the component that you want to get the detailed data for.
newGetComponent ::
  -- | 'name'
  Prelude.Text ->
  GetComponent
newGetComponent pName_ = GetComponent' {name = pName_}

-- | The name of the component that you want to get the detailed data for.
getComponent_name :: Lens.Lens' GetComponent Prelude.Text
getComponent_name = Lens.lens (\GetComponent' {name} -> name) (\s@GetComponent' {} a -> s {name = a} :: GetComponent)

instance Core.AWSRequest GetComponent where
  type AWSResponse GetComponent = GetComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentResponse'
            Prelude.<$> (x Data..?> "component")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponent where
  hashWithSalt _salt GetComponent' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetComponent where
  rnf GetComponent' {..} = Prelude.rnf name

instance Data.ToHeaders GetComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComponent where
  toJSON GetComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery GetComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComponentResponse' smart constructor.
data GetComponentResponse = GetComponentResponse'
  { -- | The detailed data of the requested component.
    component :: Prelude.Maybe Component,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'component', 'getComponentResponse_component' - The detailed data of the requested component.
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

-- | The detailed data of the requested component.
getComponentResponse_component :: Lens.Lens' GetComponentResponse (Prelude.Maybe Component)
getComponentResponse_component = Lens.lens (\GetComponentResponse' {component} -> component) (\s@GetComponentResponse' {} a -> s {component = a} :: GetComponentResponse)

-- | The response's http status code.
getComponentResponse_httpStatus :: Lens.Lens' GetComponentResponse Prelude.Int
getComponentResponse_httpStatus = Lens.lens (\GetComponentResponse' {httpStatus} -> httpStatus) (\s@GetComponentResponse' {} a -> s {httpStatus = a} :: GetComponentResponse)

instance Prelude.NFData GetComponentResponse where
  rnf GetComponentResponse' {..} =
    Prelude.rnf component `Prelude.seq`
      Prelude.rnf httpStatus
