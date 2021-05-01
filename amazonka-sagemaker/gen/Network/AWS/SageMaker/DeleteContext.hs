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
-- Module      : Network.AWS.SageMaker.DeleteContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an context.
module Network.AWS.SageMaker.DeleteContext
  ( -- * Creating a Request
    DeleteContext (..),
    newDeleteContext,

    -- * Request Lenses
    deleteContext_contextName,

    -- * Destructuring the Response
    DeleteContextResponse (..),
    newDeleteContextResponse,

    -- * Response Lenses
    deleteContextResponse_contextArn,
    deleteContextResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteContext' smart constructor.
data DeleteContext = DeleteContext'
  { -- | The name of the context to delete.
    contextName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextName', 'deleteContext_contextName' - The name of the context to delete.
newDeleteContext ::
  -- | 'contextName'
  Prelude.Text ->
  DeleteContext
newDeleteContext pContextName_ =
  DeleteContext' {contextName = pContextName_}

-- | The name of the context to delete.
deleteContext_contextName :: Lens.Lens' DeleteContext Prelude.Text
deleteContext_contextName = Lens.lens (\DeleteContext' {contextName} -> contextName) (\s@DeleteContext' {} a -> s {contextName = a} :: DeleteContext)

instance Prelude.AWSRequest DeleteContext where
  type Rs DeleteContext = DeleteContextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteContextResponse'
            Prelude.<$> (x Prelude..?> "ContextArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContext

instance Prelude.NFData DeleteContext

instance Prelude.ToHeaders DeleteContext where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteContext" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteContext where
  toJSON DeleteContext' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContextName" Prelude..= contextName)
          ]
      )

instance Prelude.ToPath DeleteContext where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteContext where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContextResponse' smart constructor.
data DeleteContextResponse = DeleteContextResponse'
  { -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteContextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextArn', 'deleteContextResponse_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'httpStatus', 'deleteContextResponse_httpStatus' - The response's http status code.
newDeleteContextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContextResponse
newDeleteContextResponse pHttpStatus_ =
  DeleteContextResponse'
    { contextArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the context.
deleteContextResponse_contextArn :: Lens.Lens' DeleteContextResponse (Prelude.Maybe Prelude.Text)
deleteContextResponse_contextArn = Lens.lens (\DeleteContextResponse' {contextArn} -> contextArn) (\s@DeleteContextResponse' {} a -> s {contextArn = a} :: DeleteContextResponse)

-- | The response's http status code.
deleteContextResponse_httpStatus :: Lens.Lens' DeleteContextResponse Prelude.Int
deleteContextResponse_httpStatus = Lens.lens (\DeleteContextResponse' {httpStatus} -> httpStatus) (\s@DeleteContextResponse' {} a -> s {httpStatus = a} :: DeleteContextResponse)

instance Prelude.NFData DeleteContextResponse
