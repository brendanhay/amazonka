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
-- Module      : Network.AWS.Connect.DeleteInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes the Amazon Connect instance.
module Network.AWS.Connect.DeleteInstance
  ( -- * Creating a Request
    DeleteInstance (..),
    newDeleteInstance,

    -- * Request Lenses
    deleteInstance_instanceId,

    -- * Destructuring the Response
    DeleteInstanceResponse (..),
    newDeleteInstanceResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteInstance_instanceId' - The identifier of the Amazon Connect instance.
newDeleteInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DeleteInstance
newDeleteInstance pInstanceId_ =
  DeleteInstance' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
deleteInstance_instanceId :: Lens.Lens' DeleteInstance Prelude.Text
deleteInstance_instanceId = Lens.lens (\DeleteInstance' {instanceId} -> instanceId) (\s@DeleteInstance' {} a -> s {instanceId = a} :: DeleteInstance)

instance Prelude.AWSRequest DeleteInstance where
  type Rs DeleteInstance = DeleteInstanceResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteInstanceResponse'

instance Prelude.Hashable DeleteInstance

instance Prelude.NFData DeleteInstance

instance Prelude.ToHeaders DeleteInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteInstance where
  toPath DeleteInstance' {..} =
    Prelude.mconcat
      ["/instance/", Prelude.toBS instanceId]

instance Prelude.ToQuery DeleteInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteInstanceResponse ::
  DeleteInstanceResponse
newDeleteInstanceResponse = DeleteInstanceResponse'

instance Prelude.NFData DeleteInstanceResponse
