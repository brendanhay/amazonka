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
-- Module      : Network.AWS.SESv2.PutDedicatedIpInPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Move a dedicated IP address to an existing dedicated IP pool.
--
-- The dedicated IP address that you specify must already exist, and must
-- be associated with your AWS account.
--
-- The dedicated IP pool you specify must already exist. You can create a
-- new pool by using the @CreateDedicatedIpPool@ operation.
module Network.AWS.SESv2.PutDedicatedIpInPool
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to move a dedicated IP address to a dedicated IP pool.
--
-- /See:/ 'newPutDedicatedIpInPool' smart constructor.
data PutDedicatedIpInPool = PutDedicatedIpInPool'
  { -- | The IP address that you want to move to the dedicated IP pool. The value
    -- you specify has to be a dedicated IP address that\'s associated with
    -- your AWS account.
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
-- your AWS account.
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
-- your AWS account.
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
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDedicatedIpInPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDedicatedIpInPool

instance Prelude.NFData PutDedicatedIpInPool

instance Core.ToHeaders PutDedicatedIpInPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutDedicatedIpInPool where
  toJSON PutDedicatedIpInPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DestinationPoolName" Core..= destinationPoolName)
          ]
      )

instance Core.ToPath PutDedicatedIpInPool where
  toPath PutDedicatedIpInPool' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ips/", Core.toBS ip, "/pool"]

instance Core.ToQuery PutDedicatedIpInPool where
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

instance Prelude.NFData PutDedicatedIpInPoolResponse
