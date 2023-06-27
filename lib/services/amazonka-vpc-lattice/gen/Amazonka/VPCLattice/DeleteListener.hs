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
-- Module      : Amazonka.VPCLattice.DeleteListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listener.
module Amazonka.VPCLattice.DeleteListener
  ( -- * Creating a Request
    DeleteListener (..),
    newDeleteListener,

    -- * Request Lenses
    deleteListener_listenerIdentifier,
    deleteListener_serviceIdentifier,

    -- * Destructuring the Response
    DeleteListenerResponse (..),
    newDeleteListenerResponse,

    -- * Response Lenses
    deleteListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteListener' smart constructor.
data DeleteListener = DeleteListener'
  { -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerIdentifier', 'deleteListener_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'serviceIdentifier', 'deleteListener_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newDeleteListener ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  DeleteListener
newDeleteListener
  pListenerIdentifier_
  pServiceIdentifier_ =
    DeleteListener'
      { listenerIdentifier =
          pListenerIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the listener.
deleteListener_listenerIdentifier :: Lens.Lens' DeleteListener Prelude.Text
deleteListener_listenerIdentifier = Lens.lens (\DeleteListener' {listenerIdentifier} -> listenerIdentifier) (\s@DeleteListener' {} a -> s {listenerIdentifier = a} :: DeleteListener)

-- | The ID or Amazon Resource Name (ARN) of the service.
deleteListener_serviceIdentifier :: Lens.Lens' DeleteListener Prelude.Text
deleteListener_serviceIdentifier = Lens.lens (\DeleteListener' {serviceIdentifier} -> serviceIdentifier) (\s@DeleteListener' {} a -> s {serviceIdentifier = a} :: DeleteListener)

instance Core.AWSRequest DeleteListener where
  type
    AWSResponse DeleteListener =
      DeleteListenerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteListenerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteListener where
  hashWithSalt _salt DeleteListener' {..} =
    _salt
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData DeleteListener where
  rnf DeleteListener' {..} =
    Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders DeleteListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteListener where
  toPath DeleteListener' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier
      ]

instance Data.ToQuery DeleteListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteListenerResponse' smart constructor.
data DeleteListenerResponse = DeleteListenerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteListenerResponse_httpStatus' - The response's http status code.
newDeleteListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteListenerResponse
newDeleteListenerResponse pHttpStatus_ =
  DeleteListenerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteListenerResponse_httpStatus :: Lens.Lens' DeleteListenerResponse Prelude.Int
deleteListenerResponse_httpStatus = Lens.lens (\DeleteListenerResponse' {httpStatus} -> httpStatus) (\s@DeleteListenerResponse' {} a -> s {httpStatus = a} :: DeleteListenerResponse)

instance Prelude.NFData DeleteListenerResponse where
  rnf DeleteListenerResponse' {..} =
    Prelude.rnf httpStatus
