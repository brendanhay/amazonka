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
-- Module      : Amazonka.MGN.DeleteVcenterClient
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given vCenter client by ID.
module Amazonka.MGN.DeleteVcenterClient
  ( -- * Creating a Request
    DeleteVcenterClient (..),
    newDeleteVcenterClient,

    -- * Request Lenses
    deleteVcenterClient_vcenterClientID,

    -- * Destructuring the Response
    DeleteVcenterClientResponse (..),
    newDeleteVcenterClientResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVcenterClient' smart constructor.
data DeleteVcenterClient = DeleteVcenterClient'
  { -- | ID of resource to be deleted.
    vcenterClientID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVcenterClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vcenterClientID', 'deleteVcenterClient_vcenterClientID' - ID of resource to be deleted.
newDeleteVcenterClient ::
  -- | 'vcenterClientID'
  Prelude.Text ->
  DeleteVcenterClient
newDeleteVcenterClient pVcenterClientID_ =
  DeleteVcenterClient'
    { vcenterClientID =
        pVcenterClientID_
    }

-- | ID of resource to be deleted.
deleteVcenterClient_vcenterClientID :: Lens.Lens' DeleteVcenterClient Prelude.Text
deleteVcenterClient_vcenterClientID = Lens.lens (\DeleteVcenterClient' {vcenterClientID} -> vcenterClientID) (\s@DeleteVcenterClient' {} a -> s {vcenterClientID = a} :: DeleteVcenterClient)

instance Core.AWSRequest DeleteVcenterClient where
  type
    AWSResponse DeleteVcenterClient =
      DeleteVcenterClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteVcenterClientResponse'

instance Prelude.Hashable DeleteVcenterClient where
  hashWithSalt _salt DeleteVcenterClient' {..} =
    _salt `Prelude.hashWithSalt` vcenterClientID

instance Prelude.NFData DeleteVcenterClient where
  rnf DeleteVcenterClient' {..} =
    Prelude.rnf vcenterClientID

instance Core.ToHeaders DeleteVcenterClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteVcenterClient where
  toJSON DeleteVcenterClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("vcenterClientID" Core..= vcenterClientID)
          ]
      )

instance Core.ToPath DeleteVcenterClient where
  toPath = Prelude.const "/DeleteVcenterClient"

instance Core.ToQuery DeleteVcenterClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVcenterClientResponse' smart constructor.
data DeleteVcenterClientResponse = DeleteVcenterClientResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVcenterClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVcenterClientResponse ::
  DeleteVcenterClientResponse
newDeleteVcenterClientResponse =
  DeleteVcenterClientResponse'

instance Prelude.NFData DeleteVcenterClientResponse where
  rnf _ = ()
