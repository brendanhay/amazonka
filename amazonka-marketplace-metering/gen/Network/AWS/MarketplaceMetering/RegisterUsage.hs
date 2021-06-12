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
-- Module      : Network.AWS.MarketplaceMetering.RegisterUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Paid container software products sold through AWS Marketplace must
-- integrate with the AWS Marketplace Metering Service and call the
-- RegisterUsage operation for software entitlement and metering. Free and
-- BYOL products for Amazon ECS or Amazon EKS aren\'t required to call
-- RegisterUsage, but you may choose to do so if you would like to receive
-- usage data in your seller reports. The sections below explain the
-- behavior of RegisterUsage. RegisterUsage performs two primary functions:
-- metering and entitlement.
--
-- -   /Entitlement/: RegisterUsage allows you to verify that the customer
--     running your paid software is subscribed to your product on AWS
--     Marketplace, enabling you to guard against unauthorized use. Your
--     container image that integrates with RegisterUsage is only required
--     to guard against unauthorized use at container startup, as such a
--     CustomerNotSubscribedException\/PlatformNotSupportedException will
--     only be thrown on the initial call to RegisterUsage. Subsequent
--     calls from the same Amazon ECS task instance (e.g. task-id) or
--     Amazon EKS pod will not throw a CustomerNotSubscribedException, even
--     if the customer unsubscribes while the Amazon ECS task or Amazon EKS
--     pod is still running.
--
-- -   /Metering/: RegisterUsage meters software use per ECS task, per
--     hour, or per pod for Amazon EKS with usage prorated to the second. A
--     minimum of 1 minute of usage applies to tasks that are short lived.
--     For example, if a customer has a 10 node Amazon ECS or Amazon EKS
--     cluster and a service configured as a Daemon Set, then Amazon ECS or
--     Amazon EKS will launch a task on all 10 cluster nodes and the
--     customer will be charged: (10 * hourly_rate). Metering for software
--     use is automatically handled by the AWS Marketplace Metering Control
--     Plane -- your software is not required to perform any metering
--     specific actions, other than call RegisterUsage once for metering of
--     software use to commence. The AWS Marketplace Metering Control Plane
--     will also continue to bill customers for running ECS tasks and
--     Amazon EKS pods, regardless of the customers subscription state,
--     removing the need for your software to perform entitlement checks at
--     runtime.
module Network.AWS.MarketplaceMetering.RegisterUsage
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
    registerUsageResponse_signature,
    registerUsageResponse_publicKeyRotationTimestamp,
    registerUsageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterUsage' smart constructor.
data RegisterUsage = RegisterUsage'
  { -- | (Optional) To scope down the registration to a specific running software
    -- instance and guard against replay attacks.
    nonce :: Core.Maybe Core.Text,
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code should be the same as the one used during the
    -- publishing of a new product.
    productCode :: Core.Text,
    -- | Public Key Version provided by AWS Marketplace
    publicKeyVersion :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'publicKeyVersion'
  Core.Natural ->
  RegisterUsage
newRegisterUsage pProductCode_ pPublicKeyVersion_ =
  RegisterUsage'
    { nonce = Core.Nothing,
      productCode = pProductCode_,
      publicKeyVersion = pPublicKeyVersion_
    }

-- | (Optional) To scope down the registration to a specific running software
-- instance and guard against replay attacks.
registerUsage_nonce :: Lens.Lens' RegisterUsage (Core.Maybe Core.Text)
registerUsage_nonce = Lens.lens (\RegisterUsage' {nonce} -> nonce) (\s@RegisterUsage' {} a -> s {nonce = a} :: RegisterUsage)

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code should be the same as the one used during the
-- publishing of a new product.
registerUsage_productCode :: Lens.Lens' RegisterUsage Core.Text
registerUsage_productCode = Lens.lens (\RegisterUsage' {productCode} -> productCode) (\s@RegisterUsage' {} a -> s {productCode = a} :: RegisterUsage)

-- | Public Key Version provided by AWS Marketplace
registerUsage_publicKeyVersion :: Lens.Lens' RegisterUsage Core.Natural
registerUsage_publicKeyVersion = Lens.lens (\RegisterUsage' {publicKeyVersion} -> publicKeyVersion) (\s@RegisterUsage' {} a -> s {publicKeyVersion = a} :: RegisterUsage)

instance Core.AWSRequest RegisterUsage where
  type
    AWSResponse RegisterUsage =
      RegisterUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterUsageResponse'
            Core.<$> (x Core..?> "Signature")
            Core.<*> (x Core..?> "PublicKeyRotationTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterUsage

instance Core.NFData RegisterUsage

instance Core.ToHeaders RegisterUsage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPMeteringService.RegisterUsage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterUsage where
  toJSON RegisterUsage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Nonce" Core..=) Core.<$> nonce,
            Core.Just ("ProductCode" Core..= productCode),
            Core.Just
              ("PublicKeyVersion" Core..= publicKeyVersion)
          ]
      )

instance Core.ToPath RegisterUsage where
  toPath = Core.const "/"

instance Core.ToQuery RegisterUsage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterUsageResponse' smart constructor.
data RegisterUsageResponse = RegisterUsageResponse'
  { -- | JWT Token
    signature :: Core.Maybe Core.Text,
    -- | (Optional) Only included when public key version has expired
    publicKeyRotationTimestamp :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'registerUsageResponse_signature' - JWT Token
--
-- 'publicKeyRotationTimestamp', 'registerUsageResponse_publicKeyRotationTimestamp' - (Optional) Only included when public key version has expired
--
-- 'httpStatus', 'registerUsageResponse_httpStatus' - The response's http status code.
newRegisterUsageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterUsageResponse
newRegisterUsageResponse pHttpStatus_ =
  RegisterUsageResponse'
    { signature = Core.Nothing,
      publicKeyRotationTimestamp = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | JWT Token
registerUsageResponse_signature :: Lens.Lens' RegisterUsageResponse (Core.Maybe Core.Text)
registerUsageResponse_signature = Lens.lens (\RegisterUsageResponse' {signature} -> signature) (\s@RegisterUsageResponse' {} a -> s {signature = a} :: RegisterUsageResponse)

-- | (Optional) Only included when public key version has expired
registerUsageResponse_publicKeyRotationTimestamp :: Lens.Lens' RegisterUsageResponse (Core.Maybe Core.UTCTime)
registerUsageResponse_publicKeyRotationTimestamp = Lens.lens (\RegisterUsageResponse' {publicKeyRotationTimestamp} -> publicKeyRotationTimestamp) (\s@RegisterUsageResponse' {} a -> s {publicKeyRotationTimestamp = a} :: RegisterUsageResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
registerUsageResponse_httpStatus :: Lens.Lens' RegisterUsageResponse Core.Int
registerUsageResponse_httpStatus = Lens.lens (\RegisterUsageResponse' {httpStatus} -> httpStatus) (\s@RegisterUsageResponse' {} a -> s {httpStatus = a} :: RegisterUsageResponse)

instance Core.NFData RegisterUsageResponse
