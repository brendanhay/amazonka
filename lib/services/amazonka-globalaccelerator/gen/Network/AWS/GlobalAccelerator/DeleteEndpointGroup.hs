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
-- Module      : Network.AWS.GlobalAccelerator.DeleteEndpointGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an endpoint group from a listener.
module Network.AWS.GlobalAccelerator.DeleteEndpointGroup
  ( -- * Creating a Request
    DeleteEndpointGroup (..),
    newDeleteEndpointGroup,

    -- * Request Lenses
    deleteEndpointGroup_endpointGroupArn,

    -- * Destructuring the Response
    DeleteEndpointGroupResponse (..),
    newDeleteEndpointGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEndpointGroup' smart constructor.
data DeleteEndpointGroup = DeleteEndpointGroup'
  { -- | The Amazon Resource Name (ARN) of the endpoint group to delete.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupArn', 'deleteEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to delete.
newDeleteEndpointGroup ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  DeleteEndpointGroup
newDeleteEndpointGroup pEndpointGroupArn_ =
  DeleteEndpointGroup'
    { endpointGroupArn =
        pEndpointGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the endpoint group to delete.
deleteEndpointGroup_endpointGroupArn :: Lens.Lens' DeleteEndpointGroup Prelude.Text
deleteEndpointGroup_endpointGroupArn = Lens.lens (\DeleteEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@DeleteEndpointGroup' {} a -> s {endpointGroupArn = a} :: DeleteEndpointGroup)

instance Core.AWSRequest DeleteEndpointGroup where
  type
    AWSResponse DeleteEndpointGroup =
      DeleteEndpointGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteEndpointGroupResponse'

instance Prelude.Hashable DeleteEndpointGroup

instance Prelude.NFData DeleteEndpointGroup

instance Core.ToHeaders DeleteEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.DeleteEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEndpointGroup where
  toJSON DeleteEndpointGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointGroupArn" Core..= endpointGroupArn)
          ]
      )

instance Core.ToPath DeleteEndpointGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointGroupResponse' smart constructor.
data DeleteEndpointGroupResponse = DeleteEndpointGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEndpointGroupResponse ::
  DeleteEndpointGroupResponse
newDeleteEndpointGroupResponse =
  DeleteEndpointGroupResponse'

instance Prelude.NFData DeleteEndpointGroupResponse
