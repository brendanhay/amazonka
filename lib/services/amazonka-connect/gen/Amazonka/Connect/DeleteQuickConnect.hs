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
-- Module      : Amazonka.Connect.DeleteQuickConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a quick connect.
module Amazonka.Connect.DeleteQuickConnect
  ( -- * Creating a Request
    DeleteQuickConnect (..),
    newDeleteQuickConnect,

    -- * Request Lenses
    deleteQuickConnect_instanceId,
    deleteQuickConnect_quickConnectId,

    -- * Destructuring the Response
    DeleteQuickConnectResponse (..),
    newDeleteQuickConnectResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteQuickConnect' smart constructor.
data DeleteQuickConnect = DeleteQuickConnect'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQuickConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteQuickConnect_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'quickConnectId', 'deleteQuickConnect_quickConnectId' - The identifier for the quick connect.
newDeleteQuickConnect ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'quickConnectId'
  Prelude.Text ->
  DeleteQuickConnect
newDeleteQuickConnect pInstanceId_ pQuickConnectId_ =
  DeleteQuickConnect'
    { instanceId = pInstanceId_,
      quickConnectId = pQuickConnectId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteQuickConnect_instanceId :: Lens.Lens' DeleteQuickConnect Prelude.Text
deleteQuickConnect_instanceId = Lens.lens (\DeleteQuickConnect' {instanceId} -> instanceId) (\s@DeleteQuickConnect' {} a -> s {instanceId = a} :: DeleteQuickConnect)

-- | The identifier for the quick connect.
deleteQuickConnect_quickConnectId :: Lens.Lens' DeleteQuickConnect Prelude.Text
deleteQuickConnect_quickConnectId = Lens.lens (\DeleteQuickConnect' {quickConnectId} -> quickConnectId) (\s@DeleteQuickConnect' {} a -> s {quickConnectId = a} :: DeleteQuickConnect)

instance Core.AWSRequest DeleteQuickConnect where
  type
    AWSResponse DeleteQuickConnect =
      DeleteQuickConnectResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteQuickConnectResponse'

instance Prelude.Hashable DeleteQuickConnect where
  hashWithSalt _salt DeleteQuickConnect' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` quickConnectId

instance Prelude.NFData DeleteQuickConnect where
  rnf DeleteQuickConnect' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf quickConnectId

instance Data.ToHeaders DeleteQuickConnect where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteQuickConnect where
  toPath DeleteQuickConnect' {..} =
    Prelude.mconcat
      [ "/quick-connects/",
        Data.toBS instanceId,
        "/",
        Data.toBS quickConnectId
      ]

instance Data.ToQuery DeleteQuickConnect where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQuickConnectResponse' smart constructor.
data DeleteQuickConnectResponse = DeleteQuickConnectResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQuickConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteQuickConnectResponse ::
  DeleteQuickConnectResponse
newDeleteQuickConnectResponse =
  DeleteQuickConnectResponse'

instance Prelude.NFData DeleteQuickConnectResponse where
  rnf _ = ()
