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
-- Module      : Amazonka.Shield.CreateProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables Shield Advanced for a specific Amazon Web Services resource. The
-- resource can be an Amazon CloudFront distribution, Elastic Load
-- Balancing load balancer, Global Accelerator accelerator, Elastic IP
-- Address, or an Amazon Route 53 hosted zone.
--
-- You can add protection to only a single resource with each
-- CreateProtection request. If you want to add protection to multiple
-- resources at once, use the
-- <https://console.aws.amazon.com/waf/ WAF console>. For more information
-- see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/getting-started-ddos.html Getting Started with Shield Advanced>
-- and
-- <https://docs.aws.amazon.com/waf/latest/developerguide/configure-new-protection.html Add Shield Advanced Protection to more Amazon Web Services Resources>.
module Amazonka.Shield.CreateProtection
  ( -- * Creating a Request
    CreateProtection (..),
    newCreateProtection,

    -- * Request Lenses
    createProtection_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newCreateProtection' smart constructor.
data CreateProtection = CreateProtection'
  { -- | One or more tag key-value pairs for the Protection object that is
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | Friendly name for the @Protection@ you are creating.
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
    -- -   For an Amazon CloudFront distribution:
    --     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
    --
    -- -   For an Global Accelerator accelerator:
    --     @arn:aws:globalaccelerator::account-id:accelerator\/accelerator-id @
    --
    -- -   For Amazon Route 53: @arn:aws:route53:::hostedzone\/hosted-zone-id @
    --
    -- -   For an Elastic IP address:
    --     @arn:aws:ec2:region:account-id:eip-allocation\/allocation-id @
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createProtection_tags' - One or more tag key-value pairs for the Protection object that is
-- created.
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
-- -   For an Amazon CloudFront distribution:
--     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
--
-- -   For an Global Accelerator accelerator:
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
    { tags = Prelude.Nothing,
      name = pName_,
      resourceArn = pResourceArn_
    }

-- | One or more tag key-value pairs for the Protection object that is
-- created.
createProtection_tags :: Lens.Lens' CreateProtection (Prelude.Maybe [Tag])
createProtection_tags = Lens.lens (\CreateProtection' {tags} -> tags) (\s@CreateProtection' {} a -> s {tags = a} :: CreateProtection) Prelude.. Lens.mapping Lens.coerced

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
-- -   For an Amazon CloudFront distribution:
--     @arn:aws:cloudfront::account-id:distribution\/distribution-id @
--
-- -   For an Global Accelerator accelerator:
--     @arn:aws:globalaccelerator::account-id:accelerator\/accelerator-id @
--
-- -   For Amazon Route 53: @arn:aws:route53:::hostedzone\/hosted-zone-id @
--
-- -   For an Elastic IP address:
--     @arn:aws:ec2:region:account-id:eip-allocation\/allocation-id @
createProtection_resourceArn :: Lens.Lens' CreateProtection Prelude.Text
createProtection_resourceArn = Lens.lens (\CreateProtection' {resourceArn} -> resourceArn) (\s@CreateProtection' {} a -> s {resourceArn = a} :: CreateProtection)

instance Core.AWSRequest CreateProtection where
  type
    AWSResponse CreateProtection =
      CreateProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProtectionResponse'
            Prelude.<$> (x Core..?> "ProtectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProtection where
  hashWithSalt salt' CreateProtection' {..} =
    salt' `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateProtection where
  rnf CreateProtection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.CreateProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProtection where
  toJSON CreateProtection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath CreateProtection where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProtectionResponse' smart constructor.
data CreateProtectionResponse = CreateProtectionResponse'
  { -- | The unique identifier (ID) for the Protection object that is created.
    protectionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateProtectionResponse where
  rnf CreateProtectionResponse' {..} =
    Prelude.rnf protectionId
      `Prelude.seq` Prelude.rnf httpStatus
