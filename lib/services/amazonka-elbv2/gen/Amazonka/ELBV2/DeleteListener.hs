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
-- Module      : Amazonka.ELBV2.DeleteListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listener.
--
-- Alternatively, your listener is deleted when you delete the load
-- balancer to which it is attached.
module Amazonka.ELBV2.DeleteListener
  ( -- * Creating a Request
    DeleteListener (..),
    newDeleteListener,

    -- * Request Lenses
    deleteListener_listenerArn,

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
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteListener' smart constructor.
data DeleteListener = DeleteListener'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text
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
-- 'listenerArn', 'deleteListener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
newDeleteListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  DeleteListener
newDeleteListener pListenerArn_ =
  DeleteListener' {listenerArn = pListenerArn_}

-- | The Amazon Resource Name (ARN) of the listener.
deleteListener_listenerArn :: Lens.Lens' DeleteListener Prelude.Text
deleteListener_listenerArn = Lens.lens (\DeleteListener' {listenerArn} -> listenerArn) (\s@DeleteListener' {} a -> s {listenerArn = a} :: DeleteListener)

instance Core.AWSRequest DeleteListener where
  type
    AWSResponse DeleteListener =
      DeleteListenerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteListenerResult"
      ( \s h x ->
          DeleteListenerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteListener where
  hashWithSalt _salt DeleteListener' {..} =
    _salt `Prelude.hashWithSalt` listenerArn

instance Prelude.NFData DeleteListener where
  rnf DeleteListener' {..} = Prelude.rnf listenerArn

instance Data.ToHeaders DeleteListener where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteListener where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteListener where
  toQuery DeleteListener' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteListener" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArn" Data.=: listenerArn
      ]

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
