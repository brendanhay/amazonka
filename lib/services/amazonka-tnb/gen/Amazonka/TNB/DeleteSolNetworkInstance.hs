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
-- Module      : Amazonka.TNB.DeleteSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- To delete a network instance, the instance must be in a stopped or
-- terminated state. To terminate a network instance, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_TerminateSolNetworkInstance.html TerminateSolNetworkInstance>.
module Amazonka.TNB.DeleteSolNetworkInstance
  ( -- * Creating a Request
    DeleteSolNetworkInstance (..),
    newDeleteSolNetworkInstance,

    -- * Request Lenses
    deleteSolNetworkInstance_nsInstanceId,

    -- * Destructuring the Response
    DeleteSolNetworkInstanceResponse (..),
    newDeleteSolNetworkInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newDeleteSolNetworkInstance' smart constructor.
data DeleteSolNetworkInstance = DeleteSolNetworkInstance'
  { -- | Network instance ID.
    nsInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsInstanceId', 'deleteSolNetworkInstance_nsInstanceId' - Network instance ID.
newDeleteSolNetworkInstance ::
  -- | 'nsInstanceId'
  Prelude.Text ->
  DeleteSolNetworkInstance
newDeleteSolNetworkInstance pNsInstanceId_ =
  DeleteSolNetworkInstance'
    { nsInstanceId =
        pNsInstanceId_
    }

-- | Network instance ID.
deleteSolNetworkInstance_nsInstanceId :: Lens.Lens' DeleteSolNetworkInstance Prelude.Text
deleteSolNetworkInstance_nsInstanceId = Lens.lens (\DeleteSolNetworkInstance' {nsInstanceId} -> nsInstanceId) (\s@DeleteSolNetworkInstance' {} a -> s {nsInstanceId = a} :: DeleteSolNetworkInstance)

instance Core.AWSRequest DeleteSolNetworkInstance where
  type
    AWSResponse DeleteSolNetworkInstance =
      DeleteSolNetworkInstanceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteSolNetworkInstanceResponse'

instance Prelude.Hashable DeleteSolNetworkInstance where
  hashWithSalt _salt DeleteSolNetworkInstance' {..} =
    _salt `Prelude.hashWithSalt` nsInstanceId

instance Prelude.NFData DeleteSolNetworkInstance where
  rnf DeleteSolNetworkInstance' {..} =
    Prelude.rnf nsInstanceId

instance Data.ToHeaders DeleteSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSolNetworkInstance where
  toPath DeleteSolNetworkInstance' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_instances/",
        Data.toBS nsInstanceId
      ]

instance Data.ToQuery DeleteSolNetworkInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSolNetworkInstanceResponse' smart constructor.
data DeleteSolNetworkInstanceResponse = DeleteSolNetworkInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSolNetworkInstanceResponse ::
  DeleteSolNetworkInstanceResponse
newDeleteSolNetworkInstanceResponse =
  DeleteSolNetworkInstanceResponse'

instance
  Prelude.NFData
    DeleteSolNetworkInstanceResponse
  where
  rnf _ = ()
