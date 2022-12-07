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
-- Module      : Amazonka.DrS.DeleteRecoveryInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single Recovery Instance by ID. This deletes the Recovery
-- Instance resource from Elastic Disaster Recovery. The Recovery Instance
-- must be disconnected first in order to delete it.
module Amazonka.DrS.DeleteRecoveryInstance
  ( -- * Creating a Request
    DeleteRecoveryInstance (..),
    newDeleteRecoveryInstance,

    -- * Request Lenses
    deleteRecoveryInstance_recoveryInstanceID,

    -- * Destructuring the Response
    DeleteRecoveryInstanceResponse (..),
    newDeleteRecoveryInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecoveryInstance' smart constructor.
data DeleteRecoveryInstance = DeleteRecoveryInstance'
  { -- | The ID of the Recovery Instance to be deleted.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceID', 'deleteRecoveryInstance_recoveryInstanceID' - The ID of the Recovery Instance to be deleted.
newDeleteRecoveryInstance ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  DeleteRecoveryInstance
newDeleteRecoveryInstance pRecoveryInstanceID_ =
  DeleteRecoveryInstance'
    { recoveryInstanceID =
        pRecoveryInstanceID_
    }

-- | The ID of the Recovery Instance to be deleted.
deleteRecoveryInstance_recoveryInstanceID :: Lens.Lens' DeleteRecoveryInstance Prelude.Text
deleteRecoveryInstance_recoveryInstanceID = Lens.lens (\DeleteRecoveryInstance' {recoveryInstanceID} -> recoveryInstanceID) (\s@DeleteRecoveryInstance' {} a -> s {recoveryInstanceID = a} :: DeleteRecoveryInstance)

instance Core.AWSRequest DeleteRecoveryInstance where
  type
    AWSResponse DeleteRecoveryInstance =
      DeleteRecoveryInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRecoveryInstanceResponse'

instance Prelude.Hashable DeleteRecoveryInstance where
  hashWithSalt _salt DeleteRecoveryInstance' {..} =
    _salt `Prelude.hashWithSalt` recoveryInstanceID

instance Prelude.NFData DeleteRecoveryInstance where
  rnf DeleteRecoveryInstance' {..} =
    Prelude.rnf recoveryInstanceID

instance Data.ToHeaders DeleteRecoveryInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRecoveryInstance where
  toJSON DeleteRecoveryInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceID" Data..= recoveryInstanceID)
          ]
      )

instance Data.ToPath DeleteRecoveryInstance where
  toPath = Prelude.const "/DeleteRecoveryInstance"

instance Data.ToQuery DeleteRecoveryInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecoveryInstanceResponse' smart constructor.
data DeleteRecoveryInstanceResponse = DeleteRecoveryInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecoveryInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecoveryInstanceResponse ::
  DeleteRecoveryInstanceResponse
newDeleteRecoveryInstanceResponse =
  DeleteRecoveryInstanceResponse'

instance
  Prelude.NFData
    DeleteRecoveryInstanceResponse
  where
  rnf _ = ()
