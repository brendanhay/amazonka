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
-- Module      : Amazonka.ImageBuilder.GetComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a component object.
module Amazonka.ImageBuilder.GetComponent
  ( -- * Creating a Request
    GetComponent (..),
    newGetComponent,

    -- * Request Lenses
    getComponent_componentBuildVersionArn,

    -- * Destructuring the Response
    GetComponentResponse (..),
    newGetComponentResponse,

    -- * Response Lenses
    getComponentResponse_component,
    getComponentResponse_requestId,
    getComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComponent' smart constructor.
data GetComponent = GetComponent'
  { -- | The Amazon Resource Name (ARN) of the component that you want to
    -- retrieve. Regex requires \"\/\\d+$\" suffix.
    componentBuildVersionArn :: Prelude.Text
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
-- 'componentBuildVersionArn', 'getComponent_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the component that you want to
-- retrieve. Regex requires \"\/\\d+$\" suffix.
newGetComponent ::
  -- | 'componentBuildVersionArn'
  Prelude.Text ->
  GetComponent
newGetComponent pComponentBuildVersionArn_ =
  GetComponent'
    { componentBuildVersionArn =
        pComponentBuildVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the component that you want to
-- retrieve. Regex requires \"\/\\d+$\" suffix.
getComponent_componentBuildVersionArn :: Lens.Lens' GetComponent Prelude.Text
getComponent_componentBuildVersionArn = Lens.lens (\GetComponent' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@GetComponent' {} a -> s {componentBuildVersionArn = a} :: GetComponent)

instance Core.AWSRequest GetComponent where
  type AWSResponse GetComponent = GetComponentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentResponse'
            Prelude.<$> (x Data..?> "component")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComponent where
  hashWithSalt _salt GetComponent' {..} =
    _salt
      `Prelude.hashWithSalt` componentBuildVersionArn

instance Prelude.NFData GetComponent where
  rnf GetComponent' {..} =
    Prelude.rnf componentBuildVersionArn

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

instance Data.ToPath GetComponent where
  toPath = Prelude.const "/GetComponent"

instance Data.ToQuery GetComponent where
  toQuery GetComponent' {..} =
    Prelude.mconcat
      [ "componentBuildVersionArn"
          Data.=: componentBuildVersionArn
      ]

-- | /See:/ 'newGetComponentResponse' smart constructor.
data GetComponentResponse = GetComponentResponse'
  { -- | The component object associated with the specified ARN.
    component :: Prelude.Maybe Component,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'component', 'getComponentResponse_component' - The component object associated with the specified ARN.
--
-- 'requestId', 'getComponentResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'getComponentResponse_httpStatus' - The response's http status code.
newGetComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComponentResponse
newGetComponentResponse pHttpStatus_ =
  GetComponentResponse'
    { component = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The component object associated with the specified ARN.
getComponentResponse_component :: Lens.Lens' GetComponentResponse (Prelude.Maybe Component)
getComponentResponse_component = Lens.lens (\GetComponentResponse' {component} -> component) (\s@GetComponentResponse' {} a -> s {component = a} :: GetComponentResponse)

-- | The request ID that uniquely identifies this request.
getComponentResponse_requestId :: Lens.Lens' GetComponentResponse (Prelude.Maybe Prelude.Text)
getComponentResponse_requestId = Lens.lens (\GetComponentResponse' {requestId} -> requestId) (\s@GetComponentResponse' {} a -> s {requestId = a} :: GetComponentResponse)

-- | The response's http status code.
getComponentResponse_httpStatus :: Lens.Lens' GetComponentResponse Prelude.Int
getComponentResponse_httpStatus = Lens.lens (\GetComponentResponse' {httpStatus} -> httpStatus) (\s@GetComponentResponse' {} a -> s {httpStatus = a} :: GetComponentResponse)

instance Prelude.NFData GetComponentResponse where
  rnf GetComponentResponse' {..} =
    Prelude.rnf component
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
