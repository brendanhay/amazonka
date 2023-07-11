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
-- Module      : Amazonka.ImageBuilder.DeleteComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a component build version.
module Amazonka.ImageBuilder.DeleteComponent
  ( -- * Creating a Request
    DeleteComponent (..),
    newDeleteComponent,

    -- * Request Lenses
    deleteComponent_componentBuildVersionArn,

    -- * Destructuring the Response
    DeleteComponentResponse (..),
    newDeleteComponentResponse,

    -- * Response Lenses
    deleteComponentResponse_componentBuildVersionArn,
    deleteComponentResponse_requestId,
    deleteComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteComponent' smart constructor.
data DeleteComponent = DeleteComponent'
  { -- | The Amazon Resource Name (ARN) of the component build version to delete.
    componentBuildVersionArn :: Prelude.Text
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
-- 'componentBuildVersionArn', 'deleteComponent_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the component build version to delete.
newDeleteComponent ::
  -- | 'componentBuildVersionArn'
  Prelude.Text ->
  DeleteComponent
newDeleteComponent pComponentBuildVersionArn_ =
  DeleteComponent'
    { componentBuildVersionArn =
        pComponentBuildVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the component build version to delete.
deleteComponent_componentBuildVersionArn :: Lens.Lens' DeleteComponent Prelude.Text
deleteComponent_componentBuildVersionArn = Lens.lens (\DeleteComponent' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@DeleteComponent' {} a -> s {componentBuildVersionArn = a} :: DeleteComponent)

instance Core.AWSRequest DeleteComponent where
  type
    AWSResponse DeleteComponent =
      DeleteComponentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteComponentResponse'
            Prelude.<$> (x Data..?> "componentBuildVersionArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteComponent where
  hashWithSalt _salt DeleteComponent' {..} =
    _salt
      `Prelude.hashWithSalt` componentBuildVersionArn

instance Prelude.NFData DeleteComponent where
  rnf DeleteComponent' {..} =
    Prelude.rnf componentBuildVersionArn

instance Data.ToHeaders DeleteComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteComponent where
  toPath = Prelude.const "/DeleteComponent"

instance Data.ToQuery DeleteComponent where
  toQuery DeleteComponent' {..} =
    Prelude.mconcat
      [ "componentBuildVersionArn"
          Data.=: componentBuildVersionArn
      ]

-- | /See:/ 'newDeleteComponentResponse' smart constructor.
data DeleteComponentResponse = DeleteComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the component build version that was
    -- deleted.
    componentBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
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
-- 'componentBuildVersionArn', 'deleteComponentResponse_componentBuildVersionArn' - The Amazon Resource Name (ARN) of the component build version that was
-- deleted.
--
-- 'requestId', 'deleteComponentResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'deleteComponentResponse_httpStatus' - The response's http status code.
newDeleteComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteComponentResponse
newDeleteComponentResponse pHttpStatus_ =
  DeleteComponentResponse'
    { componentBuildVersionArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the component build version that was
-- deleted.
deleteComponentResponse_componentBuildVersionArn :: Lens.Lens' DeleteComponentResponse (Prelude.Maybe Prelude.Text)
deleteComponentResponse_componentBuildVersionArn = Lens.lens (\DeleteComponentResponse' {componentBuildVersionArn} -> componentBuildVersionArn) (\s@DeleteComponentResponse' {} a -> s {componentBuildVersionArn = a} :: DeleteComponentResponse)

-- | The request ID that uniquely identifies this request.
deleteComponentResponse_requestId :: Lens.Lens' DeleteComponentResponse (Prelude.Maybe Prelude.Text)
deleteComponentResponse_requestId = Lens.lens (\DeleteComponentResponse' {requestId} -> requestId) (\s@DeleteComponentResponse' {} a -> s {requestId = a} :: DeleteComponentResponse)

-- | The response's http status code.
deleteComponentResponse_httpStatus :: Lens.Lens' DeleteComponentResponse Prelude.Int
deleteComponentResponse_httpStatus = Lens.lens (\DeleteComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteComponentResponse' {} a -> s {httpStatus = a} :: DeleteComponentResponse)

instance Prelude.NFData DeleteComponentResponse where
  rnf DeleteComponentResponse' {..} =
    Prelude.rnf componentBuildVersionArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
