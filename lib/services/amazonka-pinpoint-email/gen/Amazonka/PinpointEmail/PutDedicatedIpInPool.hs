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
-- Module      : Amazonka.PinpointEmail.PutDedicatedIpInPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Move a dedicated IP address to an existing dedicated IP pool.
--
-- The dedicated IP address that you specify must already exist, and must
-- be associated with your Amazon Pinpoint account.
--
-- The dedicated IP pool you specify must already exist. You can create a
-- new pool by using the @CreateDedicatedIpPool@ operation.
module Amazonka.PinpointEmail.PutDedicatedIpInPool
  ( -- * Creating a Request
    PutDedicatedIpInPool (..),
    newPutDedicatedIpInPool,

    -- * Request Lenses
    putDedicatedIpInPool_ip,
    putDedicatedIpInPool_destinationPoolName,

    -- * Destructuring the Response
    PutDedicatedIpInPoolResponse (..),
    newPutDedicatedIpInPoolResponse,

    -- * Response Lenses
    putDedicatedIpInPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to move a dedicated IP address to a dedicated IP pool.
--
-- /See:/ 'newPutDedicatedIpInPool' smart constructor.
data PutDedicatedIpInPool = PutDedicatedIpInPool'
  { -- | The IP address that you want to move to the dedicated IP pool. The value
    -- you specify has to be a dedicated IP address that\'s associated with
    -- your Amazon Pinpoint account.
    ip :: Prelude.Text,
    -- | The name of the IP pool that you want to add the dedicated IP address
    -- to. You have to specify an IP pool that already exists.
    destinationPoolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpInPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'putDedicatedIpInPool_ip' - The IP address that you want to move to the dedicated IP pool. The value
-- you specify has to be a dedicated IP address that\'s associated with
-- your Amazon Pinpoint account.
--
-- 'destinationPoolName', 'putDedicatedIpInPool_destinationPoolName' - The name of the IP pool that you want to add the dedicated IP address
-- to. You have to specify an IP pool that already exists.
newPutDedicatedIpInPool ::
  -- | 'ip'
  Prelude.Text ->
  -- | 'destinationPoolName'
  Prelude.Text ->
  PutDedicatedIpInPool
newPutDedicatedIpInPool pIp_ pDestinationPoolName_ =
  PutDedicatedIpInPool'
    { ip = pIp_,
      destinationPoolName = pDestinationPoolName_
    }

-- | The IP address that you want to move to the dedicated IP pool. The value
-- you specify has to be a dedicated IP address that\'s associated with
-- your Amazon Pinpoint account.
putDedicatedIpInPool_ip :: Lens.Lens' PutDedicatedIpInPool Prelude.Text
putDedicatedIpInPool_ip = Lens.lens (\PutDedicatedIpInPool' {ip} -> ip) (\s@PutDedicatedIpInPool' {} a -> s {ip = a} :: PutDedicatedIpInPool)

-- | The name of the IP pool that you want to add the dedicated IP address
-- to. You have to specify an IP pool that already exists.
putDedicatedIpInPool_destinationPoolName :: Lens.Lens' PutDedicatedIpInPool Prelude.Text
putDedicatedIpInPool_destinationPoolName = Lens.lens (\PutDedicatedIpInPool' {destinationPoolName} -> destinationPoolName) (\s@PutDedicatedIpInPool' {} a -> s {destinationPoolName = a} :: PutDedicatedIpInPool)

instance Core.AWSRequest PutDedicatedIpInPool where
  type
    AWSResponse PutDedicatedIpInPool =
      PutDedicatedIpInPoolResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDedicatedIpInPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDedicatedIpInPool where
  hashWithSalt _salt PutDedicatedIpInPool' {..} =
    _salt
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` destinationPoolName

instance Prelude.NFData PutDedicatedIpInPool where
  rnf PutDedicatedIpInPool' {..} =
    Prelude.rnf ip
      `Prelude.seq` Prelude.rnf destinationPoolName

instance Data.ToHeaders PutDedicatedIpInPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDedicatedIpInPool where
  toJSON PutDedicatedIpInPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DestinationPoolName" Data..= destinationPoolName)
          ]
      )

instance Data.ToPath PutDedicatedIpInPool where
  toPath PutDedicatedIpInPool' {..} =
    Prelude.mconcat
      ["/v1/email/dedicated-ips/", Data.toBS ip, "/pool"]

instance Data.ToQuery PutDedicatedIpInPool where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutDedicatedIpInPoolResponse' smart constructor.
data PutDedicatedIpInPoolResponse = PutDedicatedIpInPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpInPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDedicatedIpInPoolResponse_httpStatus' - The response's http status code.
newPutDedicatedIpInPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDedicatedIpInPoolResponse
newPutDedicatedIpInPoolResponse pHttpStatus_ =
  PutDedicatedIpInPoolResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putDedicatedIpInPoolResponse_httpStatus :: Lens.Lens' PutDedicatedIpInPoolResponse Prelude.Int
putDedicatedIpInPoolResponse_httpStatus = Lens.lens (\PutDedicatedIpInPoolResponse' {httpStatus} -> httpStatus) (\s@PutDedicatedIpInPoolResponse' {} a -> s {httpStatus = a} :: PutDedicatedIpInPoolResponse)

instance Prelude.NFData PutDedicatedIpInPoolResponse where
  rnf PutDedicatedIpInPoolResponse' {..} =
    Prelude.rnf httpStatus
