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
-- Module      : Amazonka.TNB.CancelSolNetworkOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a network operation.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
module Amazonka.TNB.CancelSolNetworkOperation
  ( -- * Creating a Request
    CancelSolNetworkOperation (..),
    newCancelSolNetworkOperation,

    -- * Request Lenses
    cancelSolNetworkOperation_nsLcmOpOccId,

    -- * Destructuring the Response
    CancelSolNetworkOperationResponse (..),
    newCancelSolNetworkOperationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newCancelSolNetworkOperation' smart constructor.
data CancelSolNetworkOperation = CancelSolNetworkOperation'
  { -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSolNetworkOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsLcmOpOccId', 'cancelSolNetworkOperation_nsLcmOpOccId' - The identifier of the network operation.
newCancelSolNetworkOperation ::
  -- | 'nsLcmOpOccId'
  Prelude.Text ->
  CancelSolNetworkOperation
newCancelSolNetworkOperation pNsLcmOpOccId_ =
  CancelSolNetworkOperation'
    { nsLcmOpOccId =
        pNsLcmOpOccId_
    }

-- | The identifier of the network operation.
cancelSolNetworkOperation_nsLcmOpOccId :: Lens.Lens' CancelSolNetworkOperation Prelude.Text
cancelSolNetworkOperation_nsLcmOpOccId = Lens.lens (\CancelSolNetworkOperation' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@CancelSolNetworkOperation' {} a -> s {nsLcmOpOccId = a} :: CancelSolNetworkOperation)

instance Core.AWSRequest CancelSolNetworkOperation where
  type
    AWSResponse CancelSolNetworkOperation =
      CancelSolNetworkOperationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      CancelSolNetworkOperationResponse'

instance Prelude.Hashable CancelSolNetworkOperation where
  hashWithSalt _salt CancelSolNetworkOperation' {..} =
    _salt `Prelude.hashWithSalt` nsLcmOpOccId

instance Prelude.NFData CancelSolNetworkOperation where
  rnf CancelSolNetworkOperation' {..} =
    Prelude.rnf nsLcmOpOccId

instance Data.ToHeaders CancelSolNetworkOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelSolNetworkOperation where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelSolNetworkOperation where
  toPath CancelSolNetworkOperation' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_lcm_op_occs/",
        Data.toBS nsLcmOpOccId,
        "/cancel"
      ]

instance Data.ToQuery CancelSolNetworkOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelSolNetworkOperationResponse' smart constructor.
data CancelSolNetworkOperationResponse = CancelSolNetworkOperationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSolNetworkOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelSolNetworkOperationResponse ::
  CancelSolNetworkOperationResponse
newCancelSolNetworkOperationResponse =
  CancelSolNetworkOperationResponse'

instance
  Prelude.NFData
    CancelSolNetworkOperationResponse
  where
  rnf _ = ()
