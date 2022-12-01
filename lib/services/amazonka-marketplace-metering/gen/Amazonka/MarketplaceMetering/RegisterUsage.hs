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
-- Module      : Amazonka.MarketplaceMetering.RegisterUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Paid container software products sold through AWS Marketplace must
-- integrate with the AWS Marketplace Metering Service and call the
-- @RegisterUsage@ operation for software entitlement and metering. Free
-- and BYOL products for Amazon ECS or Amazon EKS aren\'t required to call
-- @RegisterUsage@, but you may choose to do so if you would like to
-- receive usage data in your seller reports. The sections below explain
-- the behavior of @RegisterUsage@. @RegisterUsage@ performs two primary
-- functions: metering and entitlement.
--
-- -   /Entitlement/: @RegisterUsage@ allows you to verify that the
--     customer running your paid software is subscribed to your product on
--     AWS Marketplace, enabling you to guard against unauthorized use.
--     Your container image that integrates with @RegisterUsage@ is only
--     required to guard against unauthorized use at container startup, as
--     such a @CustomerNotSubscribedException@ or
--     @PlatformNotSupportedException@ will only be thrown on the initial
--     call to @RegisterUsage@. Subsequent calls from the same Amazon ECS
--     task instance (e.g. task-id) or Amazon EKS pod will not throw a
--     @CustomerNotSubscribedException@, even if the customer unsubscribes
--     while the Amazon ECS task or Amazon EKS pod is still running.
--
-- -   /Metering/: @RegisterUsage@ meters software use per ECS task, per
--     hour, or per pod for Amazon EKS with usage prorated to the second. A
--     minimum of 1 minute of usage applies to tasks that are short lived.
--     For example, if a customer has a 10 node Amazon ECS or Amazon EKS
--     cluster and a service configured as a Daemon Set, then Amazon ECS or
--     Amazon EKS will launch a task on all 10 cluster nodes and the
--     customer will be charged: (10 * hourly_rate). Metering for software
--     use is automatically handled by the AWS Marketplace Metering Control
--     Plane -- your software is not required to perform any metering
--     specific actions, other than call @RegisterUsage@ once for metering
--     of software use to commence. The AWS Marketplace Metering Control
--     Plane will also continue to bill customers for running ECS tasks and
--     Amazon EKS pods, regardless of the customers subscription state,
--     removing the need for your software to perform entitlement checks at
--     runtime.
module Amazonka.MarketplaceMetering.RegisterUsage
  ( -- * Creating a Request
    RegisterUsage (..),
    newRegisterUsage,

    -- * Request Lenses
    registerUsage_nonce,
    registerUsage_productCode,
    registerUsage_publicKeyVersion,

    -- * Destructuring the Response
    RegisterUsageResponse (..),
    newRegisterUsageResponse,

    -- * Response Lenses
    registerUsageResponse_publicKeyRotationTimestamp,
    registerUsageResponse_signature,
    registerUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceMetering.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterUsage' smart constructor.
data RegisterUsage = RegisterUsage'
  { -- | (Optional) To scope down the registration to a specific running software
    -- instance and guard against replay attacks.
    nonce :: Prelude.Maybe Prelude.Text,
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code should be the same as the one used during the
    -- publishing of a new product.
    productCode :: Prelude.Text,
    -- | Public Key Version provided by AWS Marketplace
    publicKeyVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonce', 'registerUsage_nonce' - (Optional) To scope down the registration to a specific running software
-- instance and guard against replay attacks.
--
-- 'productCode', 'registerUsage_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
--
-- 'publicKeyVersion', 'registerUsage_publicKeyVersion' - Public Key Version provided by AWS Marketplace
newRegisterUsage ::
  -- | 'productCode'
  Prelude.Text ->
  -- | 'publicKeyVersion'
  Prelude.Natural ->
  RegisterUsage
newRegisterUsage pProductCode_ pPublicKeyVersion_ =
  RegisterUsage'
    { nonce = Prelude.Nothing,
      productCode = pProductCode_,
      publicKeyVersion = pPublicKeyVersion_
    }

-- | (Optional) To scope down the registration to a specific running software
-- instance and guard against replay attacks.
registerUsage_nonce :: Lens.Lens' RegisterUsage (Prelude.Maybe Prelude.Text)
registerUsage_nonce = Lens.lens (\RegisterUsage' {nonce} -> nonce) (\s@RegisterUsage' {} a -> s {nonce = a} :: RegisterUsage)

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
registerUsage_productCode :: Lens.Lens' RegisterUsage Prelude.Text
registerUsage_productCode = Lens.lens (\RegisterUsage' {productCode} -> productCode) (\s@RegisterUsage' {} a -> s {productCode = a} :: RegisterUsage)

-- | Public Key Version provided by AWS Marketplace
registerUsage_publicKeyVersion :: Lens.Lens' RegisterUsage Prelude.Natural
registerUsage_publicKeyVersion = Lens.lens (\RegisterUsage' {publicKeyVersion} -> publicKeyVersion) (\s@RegisterUsage' {} a -> s {publicKeyVersion = a} :: RegisterUsage)

instance Core.AWSRequest RegisterUsage where
  type
    AWSResponse RegisterUsage =
      RegisterUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterUsageResponse'
            Prelude.<$> (x Core..?> "PublicKeyRotationTimestamp")
            Prelude.<*> (x Core..?> "Signature")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterUsage where
  hashWithSalt _salt RegisterUsage' {..} =
    _salt `Prelude.hashWithSalt` nonce
      `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` publicKeyVersion

instance Prelude.NFData RegisterUsage where
  rnf RegisterUsage' {..} =
    Prelude.rnf nonce
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf publicKeyVersion

instance Core.ToHeaders RegisterUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPMeteringService.RegisterUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterUsage where
  toJSON RegisterUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Nonce" Core..=) Prelude.<$> nonce,
            Prelude.Just ("ProductCode" Core..= productCode),
            Prelude.Just
              ("PublicKeyVersion" Core..= publicKeyVersion)
          ]
      )

instance Core.ToPath RegisterUsage where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterUsageResponse' smart constructor.
data RegisterUsageResponse = RegisterUsageResponse'
  { -- | (Optional) Only included when public key version has expired
    publicKeyRotationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | JWT Token
    signature :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicKeyRotationTimestamp', 'registerUsageResponse_publicKeyRotationTimestamp' - (Optional) Only included when public key version has expired
--
-- 'signature', 'registerUsageResponse_signature' - JWT Token
--
-- 'httpStatus', 'registerUsageResponse_httpStatus' - The response's http status code.
newRegisterUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterUsageResponse
newRegisterUsageResponse pHttpStatus_ =
  RegisterUsageResponse'
    { publicKeyRotationTimestamp =
        Prelude.Nothing,
      signature = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Optional) Only included when public key version has expired
registerUsageResponse_publicKeyRotationTimestamp :: Lens.Lens' RegisterUsageResponse (Prelude.Maybe Prelude.UTCTime)
registerUsageResponse_publicKeyRotationTimestamp = Lens.lens (\RegisterUsageResponse' {publicKeyRotationTimestamp} -> publicKeyRotationTimestamp) (\s@RegisterUsageResponse' {} a -> s {publicKeyRotationTimestamp = a} :: RegisterUsageResponse) Prelude.. Lens.mapping Core._Time

-- | JWT Token
registerUsageResponse_signature :: Lens.Lens' RegisterUsageResponse (Prelude.Maybe Prelude.Text)
registerUsageResponse_signature = Lens.lens (\RegisterUsageResponse' {signature} -> signature) (\s@RegisterUsageResponse' {} a -> s {signature = a} :: RegisterUsageResponse)

-- | The response's http status code.
registerUsageResponse_httpStatus :: Lens.Lens' RegisterUsageResponse Prelude.Int
registerUsageResponse_httpStatus = Lens.lens (\RegisterUsageResponse' {httpStatus} -> httpStatus) (\s@RegisterUsageResponse' {} a -> s {httpStatus = a} :: RegisterUsageResponse)

instance Prelude.NFData RegisterUsageResponse where
  rnf RegisterUsageResponse' {..} =
    Prelude.rnf publicKeyRotationTimestamp
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf httpStatus
