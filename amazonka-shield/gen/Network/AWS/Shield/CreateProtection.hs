{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.CreateProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables AWS Shield Advanced for a specific AWS resource. The resource
-- can be an Amazon CloudFront distribution, Elastic Load Balancing load
-- balancer, AWS Global Accelerator accelerator, Elastic IP Address, or an
-- Amazon Route 53 hosted zone.
--
-- You can add protection to only a single resource with each
-- CreateProtection request. If you want to add protection to multiple
-- resources at once, use the
-- <https://console.aws.amazon.com/waf/ AWS WAF console>. For more
-- information see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/getting-started-ddos.html Getting Started with AWS Shield Advanced>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/configure-new-protection.html Add AWS Shield Advanced Protection to more AWS Resources>.
module Network.AWS.Shield.CreateProtection
  ( -- * Creating a Request
    CreateProtection (..),
    newCreateProtection,

    -- * Request Lenses
    createProtection_name,
    createProtection_resourceArn,

    -- * Destructuring the Response
    CreateProtectionResponse (..),
    newCreateProtectionResponse,

    -- * Response Lenses
    createProtectionResponse_protectionId,
    createProtectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newCreateProtection' smart constructor.
data CreateProtection = CreateProtection'
  { -- | Friendly name for the @Protection@ you are creating.
    name :: Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the resource to be protected.
    --
    -- The ARN should be in one of the following formats:
    --
    -- -   For an Application Load Balancer:
    --     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
    --
    -- -   For an Elastic Load Balancer (Classic Load Balancer):
    --     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/load-balancer-name @
    --
    -- -   For an AWS CloudFront distribution:
    --     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
    --
    -- -   For an AWS Global Accelerator accelerator:
    --     @arn:aws:globalaccelerator::account-id:accelerator\/accelerator-id @
    --
    -- -   For Amazon Route 53: @arn:aws:route53:::hostedzone\/hosted-zone-id @
    --
    -- -   For an Elastic IP address:
    --     @arn:aws:ec2:region:account-id:eip-allocation\/allocation-id @
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createProtection_name' - Friendly name for the @Protection@ you are creating.
--
-- 'resourceArn', 'createProtection_resourceArn' - The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
--
-- -   For an Elastic Load Balancer (Classic Load Balancer):
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/load-balancer-name @
--
-- -   For an AWS CloudFront distribution:
--     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
--
-- -   For an AWS Global Accelerator accelerator:
--     @arn:aws:globalaccelerator::account-id:accelerator\/accelerator-id @
--
-- -   For Amazon Route 53: @arn:aws:route53:::hostedzone\/hosted-zone-id @
--
-- -   For an Elastic IP address:
--     @arn:aws:ec2:region:account-id:eip-allocation\/allocation-id @
newCreateProtection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  CreateProtection
newCreateProtection pName_ pResourceArn_ =
  CreateProtection'
    { name = pName_,
      resourceArn = pResourceArn_
    }

-- | Friendly name for the @Protection@ you are creating.
createProtection_name :: Lens.Lens' CreateProtection Prelude.Text
createProtection_name = Lens.lens (\CreateProtection' {name} -> name) (\s@CreateProtection' {} a -> s {name = a} :: CreateProtection)

-- | The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
--
-- -   For an Elastic Load Balancer (Classic Load Balancer):
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/load-balancer-name @
--
-- -   For an AWS CloudFront distribution:
--     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
--
-- -   For an AWS Global Accelerator accelerator:
--     @arn:aws:globalaccelerator::account-id:accelerator\/accelerator-id @
--
-- -   For Amazon Route 53: @arn:aws:route53:::hostedzone\/hosted-zone-id @
--
-- -   For an Elastic IP address:
--     @arn:aws:ec2:region:account-id:eip-allocation\/allocation-id @
createProtection_resourceArn :: Lens.Lens' CreateProtection Prelude.Text
createProtection_resourceArn = Lens.lens (\CreateProtection' {resourceArn} -> resourceArn) (\s@CreateProtection' {} a -> s {resourceArn = a} :: CreateProtection)

instance Prelude.AWSRequest CreateProtection where
  type Rs CreateProtection = CreateProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProtectionResponse'
            Prelude.<$> (x Prelude..?> "ProtectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProtection

instance Prelude.NFData CreateProtection

instance Prelude.ToHeaders CreateProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.CreateProtection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProtection where
  toJSON CreateProtection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("ResourceArn" Prelude..= resourceArn)
          ]
      )

instance Prelude.ToPath CreateProtection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProtectionResponse' smart constructor.
data CreateProtectionResponse = CreateProtectionResponse'
  { -- | The unique identifier (ID) for the Protection object that is created.
    protectionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'createProtectionResponse_protectionId' - The unique identifier (ID) for the Protection object that is created.
--
-- 'httpStatus', 'createProtectionResponse_httpStatus' - The response's http status code.
newCreateProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProtectionResponse
newCreateProtectionResponse pHttpStatus_ =
  CreateProtectionResponse'
    { protectionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier (ID) for the Protection object that is created.
createProtectionResponse_protectionId :: Lens.Lens' CreateProtectionResponse (Prelude.Maybe Prelude.Text)
createProtectionResponse_protectionId = Lens.lens (\CreateProtectionResponse' {protectionId} -> protectionId) (\s@CreateProtectionResponse' {} a -> s {protectionId = a} :: CreateProtectionResponse)

-- | The response's http status code.
createProtectionResponse_httpStatus :: Lens.Lens' CreateProtectionResponse Prelude.Int
createProtectionResponse_httpStatus = Lens.lens (\CreateProtectionResponse' {httpStatus} -> httpStatus) (\s@CreateProtectionResponse' {} a -> s {httpStatus = a} :: CreateProtectionResponse)

instance Prelude.NFData CreateProtectionResponse
