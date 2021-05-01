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
-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified layer. You must first stop and then delete all
-- associated instances or unassign registered instances. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html How to Delete a Layer>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeleteLayer
  ( -- * Creating a Request
    DeleteLayer (..),
    newDeleteLayer,

    -- * Request Lenses
    deleteLayer_layerId,

    -- * Destructuring the Response
    DeleteLayerResponse (..),
    newDeleteLayerResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLayer' smart constructor.
data DeleteLayer = DeleteLayer'
  { -- | The layer ID.
    layerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerId', 'deleteLayer_layerId' - The layer ID.
newDeleteLayer ::
  -- | 'layerId'
  Prelude.Text ->
  DeleteLayer
newDeleteLayer pLayerId_ =
  DeleteLayer' {layerId = pLayerId_}

-- | The layer ID.
deleteLayer_layerId :: Lens.Lens' DeleteLayer Prelude.Text
deleteLayer_layerId = Lens.lens (\DeleteLayer' {layerId} -> layerId) (\s@DeleteLayer' {} a -> s {layerId = a} :: DeleteLayer)

instance Prelude.AWSRequest DeleteLayer where
  type Rs DeleteLayer = DeleteLayerResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteLayerResponse'

instance Prelude.Hashable DeleteLayer

instance Prelude.NFData DeleteLayer

instance Prelude.ToHeaders DeleteLayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DeleteLayer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteLayer where
  toJSON DeleteLayer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("LayerId" Prelude..= layerId)]
      )

instance Prelude.ToPath DeleteLayer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLayer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLayerResponse' smart constructor.
data DeleteLayerResponse = DeleteLayerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLayerResponse ::
  DeleteLayerResponse
newDeleteLayerResponse = DeleteLayerResponse'

instance Prelude.NFData DeleteLayerResponse
