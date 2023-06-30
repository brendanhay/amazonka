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
-- Module      : Amazonka.IoTSiteWise.CreateGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway, which is a virtual or edge device that delivers
-- industrial data streams from local servers to IoT SiteWise. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/gateway-connector.html Ingesting data using a gateway>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.CreateGateway
  ( -- * Creating a Request
    CreateGateway (..),
    newCreateGateway,

    -- * Request Lenses
    createGateway_tags,
    createGateway_gatewayName,
    createGateway_gatewayPlatform,

    -- * Destructuring the Response
    CreateGatewayResponse (..),
    newCreateGatewayResponse,

    -- * Response Lenses
    createGatewayResponse_httpStatus,
    createGatewayResponse_gatewayId,
    createGatewayResponse_gatewayArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGateway' smart constructor.
data CreateGateway = CreateGateway'
  { -- | A list of key-value pairs that contain metadata for the gateway. For
    -- more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
    -- in the /IoT SiteWise User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, friendly name for the gateway.
    gatewayName :: Prelude.Text,
    -- | The gateway\'s platform. You can only specify one platform in a gateway.
    gatewayPlatform :: GatewayPlatform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGateway_tags' - A list of key-value pairs that contain metadata for the gateway. For
-- more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
--
-- 'gatewayName', 'createGateway_gatewayName' - A unique, friendly name for the gateway.
--
-- 'gatewayPlatform', 'createGateway_gatewayPlatform' - The gateway\'s platform. You can only specify one platform in a gateway.
newCreateGateway ::
  -- | 'gatewayName'
  Prelude.Text ->
  -- | 'gatewayPlatform'
  GatewayPlatform ->
  CreateGateway
newCreateGateway pGatewayName_ pGatewayPlatform_ =
  CreateGateway'
    { tags = Prelude.Nothing,
      gatewayName = pGatewayName_,
      gatewayPlatform = pGatewayPlatform_
    }

-- | A list of key-value pairs that contain metadata for the gateway. For
-- more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
createGateway_tags :: Lens.Lens' CreateGateway (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGateway_tags = Lens.lens (\CreateGateway' {tags} -> tags) (\s@CreateGateway' {} a -> s {tags = a} :: CreateGateway) Prelude.. Lens.mapping Lens.coerced

-- | A unique, friendly name for the gateway.
createGateway_gatewayName :: Lens.Lens' CreateGateway Prelude.Text
createGateway_gatewayName = Lens.lens (\CreateGateway' {gatewayName} -> gatewayName) (\s@CreateGateway' {} a -> s {gatewayName = a} :: CreateGateway)

-- | The gateway\'s platform. You can only specify one platform in a gateway.
createGateway_gatewayPlatform :: Lens.Lens' CreateGateway GatewayPlatform
createGateway_gatewayPlatform = Lens.lens (\CreateGateway' {gatewayPlatform} -> gatewayPlatform) (\s@CreateGateway' {} a -> s {gatewayPlatform = a} :: CreateGateway)

instance Core.AWSRequest CreateGateway where
  type
    AWSResponse CreateGateway =
      CreateGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "gatewayId")
            Prelude.<*> (x Data..:> "gatewayArn")
      )

instance Prelude.Hashable CreateGateway where
  hashWithSalt _salt CreateGateway' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` gatewayName
      `Prelude.hashWithSalt` gatewayPlatform

instance Prelude.NFData CreateGateway where
  rnf CreateGateway' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf gatewayName
      `Prelude.seq` Prelude.rnf gatewayPlatform

instance Data.ToHeaders CreateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGateway where
  toJSON CreateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("gatewayName" Data..= gatewayName),
            Prelude.Just
              ("gatewayPlatform" Data..= gatewayPlatform)
          ]
      )

instance Data.ToPath CreateGateway where
  toPath = Prelude.const "/20200301/gateways"

instance Data.ToQuery CreateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGatewayResponse' smart constructor.
data CreateGatewayResponse = CreateGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the gateway device. You can use this ID when you call other
    -- IoT SiteWise APIs.
    gatewayId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the gateway, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createGatewayResponse_httpStatus' - The response's http status code.
--
-- 'gatewayId', 'createGatewayResponse_gatewayId' - The ID of the gateway device. You can use this ID when you call other
-- IoT SiteWise APIs.
--
-- 'gatewayArn', 'createGatewayResponse_gatewayArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the gateway, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
newCreateGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'gatewayArn'
  Prelude.Text ->
  CreateGatewayResponse
newCreateGatewayResponse
  pHttpStatus_
  pGatewayId_
  pGatewayArn_ =
    CreateGatewayResponse'
      { httpStatus = pHttpStatus_,
        gatewayId = pGatewayId_,
        gatewayArn = pGatewayArn_
      }

-- | The response's http status code.
createGatewayResponse_httpStatus :: Lens.Lens' CreateGatewayResponse Prelude.Int
createGatewayResponse_httpStatus = Lens.lens (\CreateGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateGatewayResponse' {} a -> s {httpStatus = a} :: CreateGatewayResponse)

-- | The ID of the gateway device. You can use this ID when you call other
-- IoT SiteWise APIs.
createGatewayResponse_gatewayId :: Lens.Lens' CreateGatewayResponse Prelude.Text
createGatewayResponse_gatewayId = Lens.lens (\CreateGatewayResponse' {gatewayId} -> gatewayId) (\s@CreateGatewayResponse' {} a -> s {gatewayId = a} :: CreateGatewayResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the gateway, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
createGatewayResponse_gatewayArn :: Lens.Lens' CreateGatewayResponse Prelude.Text
createGatewayResponse_gatewayArn = Lens.lens (\CreateGatewayResponse' {gatewayArn} -> gatewayArn) (\s@CreateGatewayResponse' {} a -> s {gatewayArn = a} :: CreateGatewayResponse)

instance Prelude.NFData CreateGatewayResponse where
  rnf CreateGatewayResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf gatewayArn
