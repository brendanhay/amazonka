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
-- Module      : Amazonka.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.CreateInternetGateway
  ( -- * Creating a Request
    CreateInternetGateway (..),
    newCreateInternetGateway,

    -- * Request Lenses
    createInternetGateway_dryRun,
    createInternetGateway_tagSpecifications,

    -- * Destructuring the Response
    CreateInternetGatewayResponse (..),
    newCreateInternetGatewayResponse,

    -- * Response Lenses
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the internet gateway.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
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
-- 'dryRun', 'createInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createInternetGateway_tagSpecifications' - The tags to assign to the internet gateway.
newCreateInternetGateway ::
  CreateInternetGateway
newCreateInternetGateway =
  CreateInternetGateway'
    { dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createInternetGateway_dryRun :: Lens.Lens' CreateInternetGateway (Prelude.Maybe Prelude.Bool)
createInternetGateway_dryRun = Lens.lens (\CreateInternetGateway' {dryRun} -> dryRun) (\s@CreateInternetGateway' {} a -> s {dryRun = a} :: CreateInternetGateway)

-- | The tags to assign to the internet gateway.
createInternetGateway_tagSpecifications :: Lens.Lens' CreateInternetGateway (Prelude.Maybe [TagSpecification])
createInternetGateway_tagSpecifications = Lens.lens (\CreateInternetGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateInternetGateway' {} a -> s {tagSpecifications = a} :: CreateInternetGateway) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateInternetGateway where
  type
    AWSResponse CreateInternetGateway =
      CreateInternetGatewayResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInternetGatewayResponse'
            Prelude.<$> (x Data..@? "internetGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInternetGateway where
  hashWithSalt _salt CreateInternetGateway' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateInternetGateway where
  rnf CreateInternetGateway' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateInternetGateway where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateInternetGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInternetGateway where
  toQuery CreateInternetGateway' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateInternetGateway" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
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

instance Prelude.NFData CreateInternetGatewayResponse where
  rnf CreateInternetGatewayResponse' {..} =
    Prelude.rnf internetGateway
      `Prelude.seq` Prelude.rnf httpStatus
