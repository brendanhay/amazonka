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
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an internet gateway for use with a VPC. After creating the
-- internet gateway, you attach it to a VPC using AttachInternetGateway.
--
-- For more information about your VPC and internet gateway, see the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide>.
module Network.AWS.EC2.CreateInternetGateway
  ( -- * Creating a Request
    CreateInternetGateway (..),
    newCreateInternetGateway,

    -- * Request Lenses
    createInternetGateway_tagSpecifications,
    createInternetGateway_dryRun,

    -- * Destructuring the Response
    CreateInternetGatewayResponse (..),
    newCreateInternetGatewayResponse,

    -- * Response Lenses
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { -- | The tags to assign to the internet gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createInternetGateway_tagSpecifications' - The tags to assign to the internet gateway.
--
-- 'dryRun', 'createInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newCreateInternetGateway ::
  CreateInternetGateway
newCreateInternetGateway =
  CreateInternetGateway'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The tags to assign to the internet gateway.
createInternetGateway_tagSpecifications :: Lens.Lens' CreateInternetGateway (Prelude.Maybe [TagSpecification])
createInternetGateway_tagSpecifications = Lens.lens (\CreateInternetGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateInternetGateway' {} a -> s {tagSpecifications = a} :: CreateInternetGateway) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createInternetGateway_dryRun :: Lens.Lens' CreateInternetGateway (Prelude.Maybe Prelude.Bool)
createInternetGateway_dryRun = Lens.lens (\CreateInternetGateway' {dryRun} -> dryRun) (\s@CreateInternetGateway' {} a -> s {dryRun = a} :: CreateInternetGateway)

instance Core.AWSRequest CreateInternetGateway where
  type
    AWSResponse CreateInternetGateway =
      CreateInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInternetGatewayResponse'
            Prelude.<$> (x Core..@? "internetGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInternetGateway

instance Prelude.NFData CreateInternetGateway

instance Core.ToHeaders CreateInternetGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateInternetGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateInternetGateway where
  toQuery CreateInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateInternetGateway" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newCreateInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { -- | Information about the internet gateway.
    internetGateway :: Prelude.Maybe InternetGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'internetGateway', 'createInternetGatewayResponse_internetGateway' - Information about the internet gateway.
--
-- 'httpStatus', 'createInternetGatewayResponse_httpStatus' - The response's http status code.
newCreateInternetGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInternetGatewayResponse
newCreateInternetGatewayResponse pHttpStatus_ =
  CreateInternetGatewayResponse'
    { internetGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the internet gateway.
createInternetGatewayResponse_internetGateway :: Lens.Lens' CreateInternetGatewayResponse (Prelude.Maybe InternetGateway)
createInternetGatewayResponse_internetGateway = Lens.lens (\CreateInternetGatewayResponse' {internetGateway} -> internetGateway) (\s@CreateInternetGatewayResponse' {} a -> s {internetGateway = a} :: CreateInternetGatewayResponse)

-- | The response's http status code.
createInternetGatewayResponse_httpStatus :: Lens.Lens' CreateInternetGatewayResponse Prelude.Int
createInternetGatewayResponse_httpStatus = Lens.lens (\CreateInternetGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateInternetGatewayResponse' {} a -> s {httpStatus = a} :: CreateInternetGatewayResponse)

instance Prelude.NFData CreateInternetGatewayResponse
