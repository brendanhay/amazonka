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
-- Module      : Network.AWS.GlobalAccelerator.DeleteCustomRoutingListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a listener for a custom routing accelerator.
module Network.AWS.GlobalAccelerator.DeleteCustomRoutingListener
  ( -- * Creating a Request
    DeleteCustomRoutingListener (..),
    newDeleteCustomRoutingListener,

    -- * Request Lenses
    deleteCustomRoutingListener_listenerArn,

    -- * Destructuring the Response
    DeleteCustomRoutingListenerResponse (..),
    newDeleteCustomRoutingListenerResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCustomRoutingListener' smart constructor.
data DeleteCustomRoutingListener = DeleteCustomRoutingListener'
  { -- | The Amazon Resource Name (ARN) of the listener to delete.
    listenerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'deleteCustomRoutingListener_listenerArn' - The Amazon Resource Name (ARN) of the listener to delete.
newDeleteCustomRoutingListener ::
  -- | 'listenerArn'
  Prelude.Text ->
  DeleteCustomRoutingListener
newDeleteCustomRoutingListener pListenerArn_ =
  DeleteCustomRoutingListener'
    { listenerArn =
        pListenerArn_
    }

-- | The Amazon Resource Name (ARN) of the listener to delete.
deleteCustomRoutingListener_listenerArn :: Lens.Lens' DeleteCustomRoutingListener Prelude.Text
deleteCustomRoutingListener_listenerArn = Lens.lens (\DeleteCustomRoutingListener' {listenerArn} -> listenerArn) (\s@DeleteCustomRoutingListener' {} a -> s {listenerArn = a} :: DeleteCustomRoutingListener)

instance Core.AWSRequest DeleteCustomRoutingListener where
  type
    AWSResponse DeleteCustomRoutingListener =
      DeleteCustomRoutingListenerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteCustomRoutingListenerResponse'

instance Prelude.Hashable DeleteCustomRoutingListener

instance Prelude.NFData DeleteCustomRoutingListener

instance Core.ToHeaders DeleteCustomRoutingListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.DeleteCustomRoutingListener" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCustomRoutingListener where
  toJSON DeleteCustomRoutingListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ListenerArn" Core..= listenerArn)]
      )

instance Core.ToPath DeleteCustomRoutingListener where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCustomRoutingListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomRoutingListenerResponse' smart constructor.
data DeleteCustomRoutingListenerResponse = DeleteCustomRoutingListenerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomRoutingListenerResponse ::
  DeleteCustomRoutingListenerResponse
newDeleteCustomRoutingListenerResponse =
  DeleteCustomRoutingListenerResponse'

instance
  Prelude.NFData
    DeleteCustomRoutingListenerResponse
