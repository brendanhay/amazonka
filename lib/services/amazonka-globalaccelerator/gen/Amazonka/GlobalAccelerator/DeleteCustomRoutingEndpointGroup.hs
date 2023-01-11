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
-- Module      : Amazonka.GlobalAccelerator.DeleteCustomRoutingEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an endpoint group from a listener for a custom routing
-- accelerator.
module Amazonka.GlobalAccelerator.DeleteCustomRoutingEndpointGroup
  ( -- * Creating a Request
    DeleteCustomRoutingEndpointGroup (..),
    newDeleteCustomRoutingEndpointGroup,

    -- * Request Lenses
    deleteCustomRoutingEndpointGroup_endpointGroupArn,

    -- * Destructuring the Response
    DeleteCustomRoutingEndpointGroupResponse (..),
    newDeleteCustomRoutingEndpointGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomRoutingEndpointGroup' smart constructor.
data DeleteCustomRoutingEndpointGroup = DeleteCustomRoutingEndpointGroup'
  { -- | The Amazon Resource Name (ARN) of the endpoint group to delete.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupArn', 'deleteCustomRoutingEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to delete.
newDeleteCustomRoutingEndpointGroup ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  DeleteCustomRoutingEndpointGroup
newDeleteCustomRoutingEndpointGroup
  pEndpointGroupArn_ =
    DeleteCustomRoutingEndpointGroup'
      { endpointGroupArn =
          pEndpointGroupArn_
      }

-- | The Amazon Resource Name (ARN) of the endpoint group to delete.
deleteCustomRoutingEndpointGroup_endpointGroupArn :: Lens.Lens' DeleteCustomRoutingEndpointGroup Prelude.Text
deleteCustomRoutingEndpointGroup_endpointGroupArn = Lens.lens (\DeleteCustomRoutingEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@DeleteCustomRoutingEndpointGroup' {} a -> s {endpointGroupArn = a} :: DeleteCustomRoutingEndpointGroup)

instance
  Core.AWSRequest
    DeleteCustomRoutingEndpointGroup
  where
  type
    AWSResponse DeleteCustomRoutingEndpointGroup =
      DeleteCustomRoutingEndpointGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCustomRoutingEndpointGroupResponse'

instance
  Prelude.Hashable
    DeleteCustomRoutingEndpointGroup
  where
  hashWithSalt
    _salt
    DeleteCustomRoutingEndpointGroup' {..} =
      _salt `Prelude.hashWithSalt` endpointGroupArn

instance
  Prelude.NFData
    DeleteCustomRoutingEndpointGroup
  where
  rnf DeleteCustomRoutingEndpointGroup' {..} =
    Prelude.rnf endpointGroupArn

instance
  Data.ToHeaders
    DeleteCustomRoutingEndpointGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DeleteCustomRoutingEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCustomRoutingEndpointGroup where
  toJSON DeleteCustomRoutingEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath DeleteCustomRoutingEndpointGroup where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteCustomRoutingEndpointGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomRoutingEndpointGroupResponse' smart constructor.
data DeleteCustomRoutingEndpointGroupResponse = DeleteCustomRoutingEndpointGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomRoutingEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomRoutingEndpointGroupResponse ::
  DeleteCustomRoutingEndpointGroupResponse
newDeleteCustomRoutingEndpointGroupResponse =
  DeleteCustomRoutingEndpointGroupResponse'

instance
  Prelude.NFData
    DeleteCustomRoutingEndpointGroupResponse
  where
  rnf _ = ()
