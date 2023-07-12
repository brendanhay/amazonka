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
-- Module      : Amazonka.GlobalAccelerator.DeleteEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an endpoint group from a listener.
module Amazonka.GlobalAccelerator.DeleteEndpointGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteEndpointGroupResponse'

instance Prelude.Hashable DeleteEndpointGroup where
  hashWithSalt _salt DeleteEndpointGroup' {..} =
    _salt `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData DeleteEndpointGroup where
  rnf DeleteEndpointGroup' {..} =
    Prelude.rnf endpointGroupArn

instance Data.ToHeaders DeleteEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DeleteEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEndpointGroup where
  toJSON DeleteEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath DeleteEndpointGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEndpointGroup where
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

instance Prelude.NFData DeleteEndpointGroupResponse where
  rnf _ = ()
