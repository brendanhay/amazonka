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
-- Module      : Amazonka.AppRunner.CreateVpcConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner VPC connector resource. App Runner requires this
-- resource when you want to associate your App Runner service to a custom
-- Amazon Virtual Private Cloud (Amazon VPC).
module Amazonka.AppRunner.CreateVpcConnector
  ( -- * Creating a Request
    CreateVpcConnector (..),
    newCreateVpcConnector,

    -- * Request Lenses
    createVpcConnector_tags,
    createVpcConnector_securityGroups,
    createVpcConnector_vpcConnectorName,
    createVpcConnector_subnets,

    -- * Destructuring the Response
    CreateVpcConnectorResponse (..),
    newCreateVpcConnectorResponse,

    -- * Response Lenses
    createVpcConnectorResponse_httpStatus,
    createVpcConnectorResponse_vpcConnector,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcConnector' smart constructor.
data CreateVpcConnector = CreateVpcConnector'
  { -- | A list of metadata items that you can associate with your VPC connector
    -- resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | A list of IDs of security groups that App Runner should use for access
    -- to Amazon Web Services resources under the specified subnets. If not
    -- specified, App Runner uses the default security group of the Amazon VPC.
    -- The default security group allows all outbound traffic.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A name for the VPC connector.
    vpcConnectorName :: Prelude.Text,
    -- | A list of IDs of subnets that App Runner should use when it associates
    -- your service with a custom Amazon VPC. Specify IDs of subnets of a
    -- single Amazon VPC. App Runner determines the Amazon VPC from the subnets
    -- you specify.
    --
    -- App Runner currently only provides support for IPv4.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVpcConnector_tags' - A list of metadata items that you can associate with your VPC connector
-- resource. A tag is a key-value pair.
--
-- 'securityGroups', 'createVpcConnector_securityGroups' - A list of IDs of security groups that App Runner should use for access
-- to Amazon Web Services resources under the specified subnets. If not
-- specified, App Runner uses the default security group of the Amazon VPC.
-- The default security group allows all outbound traffic.
--
-- 'vpcConnectorName', 'createVpcConnector_vpcConnectorName' - A name for the VPC connector.
--
-- 'subnets', 'createVpcConnector_subnets' - A list of IDs of subnets that App Runner should use when it associates
-- your service with a custom Amazon VPC. Specify IDs of subnets of a
-- single Amazon VPC. App Runner determines the Amazon VPC from the subnets
-- you specify.
--
-- App Runner currently only provides support for IPv4.
newCreateVpcConnector ::
  -- | 'vpcConnectorName'
  Prelude.Text ->
  CreateVpcConnector
newCreateVpcConnector pVpcConnectorName_ =
  CreateVpcConnector'
    { tags = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      vpcConnectorName = pVpcConnectorName_,
      subnets = Prelude.mempty
    }

-- | A list of metadata items that you can associate with your VPC connector
-- resource. A tag is a key-value pair.
createVpcConnector_tags :: Lens.Lens' CreateVpcConnector (Prelude.Maybe [Tag])
createVpcConnector_tags = Lens.lens (\CreateVpcConnector' {tags} -> tags) (\s@CreateVpcConnector' {} a -> s {tags = a} :: CreateVpcConnector) Prelude.. Lens.mapping Lens.coerced

-- | A list of IDs of security groups that App Runner should use for access
-- to Amazon Web Services resources under the specified subnets. If not
-- specified, App Runner uses the default security group of the Amazon VPC.
-- The default security group allows all outbound traffic.
createVpcConnector_securityGroups :: Lens.Lens' CreateVpcConnector (Prelude.Maybe [Prelude.Text])
createVpcConnector_securityGroups = Lens.lens (\CreateVpcConnector' {securityGroups} -> securityGroups) (\s@CreateVpcConnector' {} a -> s {securityGroups = a} :: CreateVpcConnector) Prelude.. Lens.mapping Lens.coerced

-- | A name for the VPC connector.
createVpcConnector_vpcConnectorName :: Lens.Lens' CreateVpcConnector Prelude.Text
createVpcConnector_vpcConnectorName = Lens.lens (\CreateVpcConnector' {vpcConnectorName} -> vpcConnectorName) (\s@CreateVpcConnector' {} a -> s {vpcConnectorName = a} :: CreateVpcConnector)

-- | A list of IDs of subnets that App Runner should use when it associates
-- your service with a custom Amazon VPC. Specify IDs of subnets of a
-- single Amazon VPC. App Runner determines the Amazon VPC from the subnets
-- you specify.
--
-- App Runner currently only provides support for IPv4.
createVpcConnector_subnets :: Lens.Lens' CreateVpcConnector [Prelude.Text]
createVpcConnector_subnets = Lens.lens (\CreateVpcConnector' {subnets} -> subnets) (\s@CreateVpcConnector' {} a -> s {subnets = a} :: CreateVpcConnector) Prelude.. Lens.coerced

instance Core.AWSRequest CreateVpcConnector where
  type
    AWSResponse CreateVpcConnector =
      CreateVpcConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcConnector")
      )

instance Prelude.Hashable CreateVpcConnector where
  hashWithSalt _salt CreateVpcConnector' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` vpcConnectorName
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData CreateVpcConnector where
  rnf CreateVpcConnector' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf vpcConnectorName
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToHeaders CreateVpcConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.CreateVpcConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcConnector where
  toJSON CreateVpcConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just
              ("VpcConnectorName" Data..= vpcConnectorName),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )

instance Data.ToPath CreateVpcConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVpcConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcConnectorResponse' smart constructor.
data CreateVpcConnectorResponse = CreateVpcConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC connector that\'s created by this
    -- request.
    vpcConnector :: VpcConnector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVpcConnectorResponse_httpStatus' - The response's http status code.
--
-- 'vpcConnector', 'createVpcConnectorResponse_vpcConnector' - A description of the App Runner VPC connector that\'s created by this
-- request.
newCreateVpcConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcConnector'
  VpcConnector ->
  CreateVpcConnectorResponse
newCreateVpcConnectorResponse
  pHttpStatus_
  pVpcConnector_ =
    CreateVpcConnectorResponse'
      { httpStatus =
          pHttpStatus_,
        vpcConnector = pVpcConnector_
      }

-- | The response's http status code.
createVpcConnectorResponse_httpStatus :: Lens.Lens' CreateVpcConnectorResponse Prelude.Int
createVpcConnectorResponse_httpStatus = Lens.lens (\CreateVpcConnectorResponse' {httpStatus} -> httpStatus) (\s@CreateVpcConnectorResponse' {} a -> s {httpStatus = a} :: CreateVpcConnectorResponse)

-- | A description of the App Runner VPC connector that\'s created by this
-- request.
createVpcConnectorResponse_vpcConnector :: Lens.Lens' CreateVpcConnectorResponse VpcConnector
createVpcConnectorResponse_vpcConnector = Lens.lens (\CreateVpcConnectorResponse' {vpcConnector} -> vpcConnector) (\s@CreateVpcConnectorResponse' {} a -> s {vpcConnector = a} :: CreateVpcConnectorResponse)

instance Prelude.NFData CreateVpcConnectorResponse where
  rnf CreateVpcConnectorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcConnector
