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
-- Module      : Amazonka.Lightsail.DetachCertificateFromDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Lightsail.DetachCertificateFromDistribution
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachCertificateFromDistributionResponse'
            Prelude.<$> (x Data..?> "operation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachCertificateFromDistribution
  where
  hashWithSalt
    _salt
    DetachCertificateFromDistribution' {..} =
      _salt `Prelude.hashWithSalt` distributionName

instance
  Prelude.NFData
    DetachCertificateFromDistribution
  where
  rnf DetachCertificateFromDistribution' {..} =
    Prelude.rnf distributionName

instance
  Data.ToHeaders
    DetachCertificateFromDistribution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DetachCertificateFromDistribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DetachCertificateFromDistribution
  where
  toJSON DetachCertificateFromDistribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("distributionName" Data..= distributionName)
          ]
      )

instance
  Data.ToPath
    DetachCertificateFromDistribution
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
  where
  rnf DetachCertificateFromDistributionResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
