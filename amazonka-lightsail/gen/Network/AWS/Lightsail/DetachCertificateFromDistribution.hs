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
-- Module      : Network.AWS.Lightsail.DetachCertificateFromDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an SSL\/TLS certificate from your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- After the certificate is detached, your distribution stops accepting
-- traffic for all of the domains that are associated with the certificate.
module Network.AWS.Lightsail.DetachCertificateFromDistribution
  ( -- * Creating a Request
    DetachCertificateFromDistribution (..),
    newDetachCertificateFromDistribution,

    -- * Request Lenses
    detachCertificateFromDistribution_distributionName,

    -- * Destructuring the Response
    DetachCertificateFromDistributionResponse (..),
    newDetachCertificateFromDistributionResponse,

    -- * Response Lenses
    detachCertificateFromDistributionResponse_operation,
    detachCertificateFromDistributionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachCertificateFromDistribution' smart constructor.
data DetachCertificateFromDistribution = DetachCertificateFromDistribution'
  { -- | The name of the distribution from which to detach the certificate.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachCertificateFromDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'detachCertificateFromDistribution_distributionName' - The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newDetachCertificateFromDistribution ::
  -- | 'distributionName'
  Prelude.Text ->
  DetachCertificateFromDistribution
newDetachCertificateFromDistribution
  pDistributionName_ =
    DetachCertificateFromDistribution'
      { distributionName =
          pDistributionName_
      }

-- | The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
detachCertificateFromDistribution_distributionName :: Lens.Lens' DetachCertificateFromDistribution Prelude.Text
detachCertificateFromDistribution_distributionName = Lens.lens (\DetachCertificateFromDistribution' {distributionName} -> distributionName) (\s@DetachCertificateFromDistribution' {} a -> s {distributionName = a} :: DetachCertificateFromDistribution)

instance
  Core.AWSRequest
    DetachCertificateFromDistribution
  where
  type
    AWSResponse DetachCertificateFromDistribution =
      DetachCertificateFromDistributionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachCertificateFromDistributionResponse'
            Prelude.<$> (x Core..?> "operation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachCertificateFromDistribution

instance
  Prelude.NFData
    DetachCertificateFromDistribution

instance
  Core.ToHeaders
    DetachCertificateFromDistribution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DetachCertificateFromDistribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DetachCertificateFromDistribution
  where
  toJSON DetachCertificateFromDistribution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("distributionName" Core..= distributionName)
          ]
      )

instance
  Core.ToPath
    DetachCertificateFromDistribution
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DetachCertificateFromDistribution
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachCertificateFromDistributionResponse' smart constructor.
data DetachCertificateFromDistributionResponse = DetachCertificateFromDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of
    -- the request, the timestamp of the request, and the resources affected by
    -- the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachCertificateFromDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'detachCertificateFromDistributionResponse_operation' - An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
--
-- 'httpStatus', 'detachCertificateFromDistributionResponse_httpStatus' - The response's http status code.
newDetachCertificateFromDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachCertificateFromDistributionResponse
newDetachCertificateFromDistributionResponse
  pHttpStatus_ =
    DetachCertificateFromDistributionResponse'
      { operation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that describes the result of the action, such as the status of
-- the request, the timestamp of the request, and the resources affected by
-- the request.
detachCertificateFromDistributionResponse_operation :: Lens.Lens' DetachCertificateFromDistributionResponse (Prelude.Maybe Operation)
detachCertificateFromDistributionResponse_operation = Lens.lens (\DetachCertificateFromDistributionResponse' {operation} -> operation) (\s@DetachCertificateFromDistributionResponse' {} a -> s {operation = a} :: DetachCertificateFromDistributionResponse)

-- | The response's http status code.
detachCertificateFromDistributionResponse_httpStatus :: Lens.Lens' DetachCertificateFromDistributionResponse Prelude.Int
detachCertificateFromDistributionResponse_httpStatus = Lens.lens (\DetachCertificateFromDistributionResponse' {httpStatus} -> httpStatus) (\s@DetachCertificateFromDistributionResponse' {} a -> s {httpStatus = a} :: DetachCertificateFromDistributionResponse)

instance
  Prelude.NFData
    DetachCertificateFromDistributionResponse
