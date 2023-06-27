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
-- Module      : Amazonka.Kafka.CreateVpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MSK VPC connection.
module Amazonka.Kafka.CreateVpcConnection
  ( -- * Creating a Request
    CreateVpcConnection (..),
    newCreateVpcConnection,

    -- * Request Lenses
    createVpcConnection_tags,
    createVpcConnection_targetClusterArn,
    createVpcConnection_authentication,
    createVpcConnection_vpcId,
    createVpcConnection_clientSubnets,
    createVpcConnection_securityGroups,

    -- * Destructuring the Response
    CreateVpcConnectionResponse (..),
    newCreateVpcConnectionResponse,

    -- * Response Lenses
    createVpcConnectionResponse_authentication,
    createVpcConnectionResponse_clientSubnets,
    createVpcConnectionResponse_creationTime,
    createVpcConnectionResponse_securityGroups,
    createVpcConnectionResponse_state,
    createVpcConnectionResponse_tags,
    createVpcConnectionResponse_vpcConnectionArn,
    createVpcConnectionResponse_vpcId,
    createVpcConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcConnection' smart constructor.
data CreateVpcConnection = CreateVpcConnection'
  { -- | A map of tags for the VPC connection.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The cluster Amazon Resource Name (ARN) for the VPC connection.
    targetClusterArn :: Prelude.Text,
    -- | The authentication type of VPC connection.
    authentication :: Prelude.Text,
    -- | The VPC ID of VPC connection.
    vpcId :: Prelude.Text,
    -- | The list of client subnets.
    clientSubnets :: [Prelude.Text],
    -- | The list of security groups.
    securityGroups :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVpcConnection_tags' - A map of tags for the VPC connection.
--
-- 'targetClusterArn', 'createVpcConnection_targetClusterArn' - The cluster Amazon Resource Name (ARN) for the VPC connection.
--
-- 'authentication', 'createVpcConnection_authentication' - The authentication type of VPC connection.
--
-- 'vpcId', 'createVpcConnection_vpcId' - The VPC ID of VPC connection.
--
-- 'clientSubnets', 'createVpcConnection_clientSubnets' - The list of client subnets.
--
-- 'securityGroups', 'createVpcConnection_securityGroups' - The list of security groups.
newCreateVpcConnection ::
  -- | 'targetClusterArn'
  Prelude.Text ->
  -- | 'authentication'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateVpcConnection
newCreateVpcConnection
  pTargetClusterArn_
  pAuthentication_
  pVpcId_ =
    CreateVpcConnection'
      { tags = Prelude.Nothing,
        targetClusterArn = pTargetClusterArn_,
        authentication = pAuthentication_,
        vpcId = pVpcId_,
        clientSubnets = Prelude.mempty,
        securityGroups = Prelude.mempty
      }

-- | A map of tags for the VPC connection.
createVpcConnection_tags :: Lens.Lens' CreateVpcConnection (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcConnection_tags = Lens.lens (\CreateVpcConnection' {tags} -> tags) (\s@CreateVpcConnection' {} a -> s {tags = a} :: CreateVpcConnection) Prelude.. Lens.mapping Lens.coerced

-- | The cluster Amazon Resource Name (ARN) for the VPC connection.
createVpcConnection_targetClusterArn :: Lens.Lens' CreateVpcConnection Prelude.Text
createVpcConnection_targetClusterArn = Lens.lens (\CreateVpcConnection' {targetClusterArn} -> targetClusterArn) (\s@CreateVpcConnection' {} a -> s {targetClusterArn = a} :: CreateVpcConnection)

-- | The authentication type of VPC connection.
createVpcConnection_authentication :: Lens.Lens' CreateVpcConnection Prelude.Text
createVpcConnection_authentication = Lens.lens (\CreateVpcConnection' {authentication} -> authentication) (\s@CreateVpcConnection' {} a -> s {authentication = a} :: CreateVpcConnection)

-- | The VPC ID of VPC connection.
createVpcConnection_vpcId :: Lens.Lens' CreateVpcConnection Prelude.Text
createVpcConnection_vpcId = Lens.lens (\CreateVpcConnection' {vpcId} -> vpcId) (\s@CreateVpcConnection' {} a -> s {vpcId = a} :: CreateVpcConnection)

-- | The list of client subnets.
createVpcConnection_clientSubnets :: Lens.Lens' CreateVpcConnection [Prelude.Text]
createVpcConnection_clientSubnets = Lens.lens (\CreateVpcConnection' {clientSubnets} -> clientSubnets) (\s@CreateVpcConnection' {} a -> s {clientSubnets = a} :: CreateVpcConnection) Prelude.. Lens.coerced

-- | The list of security groups.
createVpcConnection_securityGroups :: Lens.Lens' CreateVpcConnection [Prelude.Text]
createVpcConnection_securityGroups = Lens.lens (\CreateVpcConnection' {securityGroups} -> securityGroups) (\s@CreateVpcConnection' {} a -> s {securityGroups = a} :: CreateVpcConnection) Prelude.. Lens.coerced

instance Core.AWSRequest CreateVpcConnection where
  type
    AWSResponse CreateVpcConnection =
      CreateVpcConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcConnectionResponse'
            Prelude.<$> (x Data..?> "authentication")
            Prelude.<*> (x Data..?> "clientSubnets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vpcConnectionArn")
            Prelude.<*> (x Data..?> "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcConnection where
  hashWithSalt _salt CreateVpcConnection' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetClusterArn
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` clientSubnets
      `Prelude.hashWithSalt` securityGroups

instance Prelude.NFData CreateVpcConnection where
  rnf CreateVpcConnection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetClusterArn
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf clientSubnets
      `Prelude.seq` Prelude.rnf securityGroups

instance Data.ToHeaders CreateVpcConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcConnection where
  toJSON CreateVpcConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("targetClusterArn" Data..= targetClusterArn),
            Prelude.Just
              ("authentication" Data..= authentication),
            Prelude.Just ("vpcId" Data..= vpcId),
            Prelude.Just ("clientSubnets" Data..= clientSubnets),
            Prelude.Just
              ("securityGroups" Data..= securityGroups)
          ]
      )

instance Data.ToPath CreateVpcConnection where
  toPath = Prelude.const "/v1/vpc-connection"

instance Data.ToQuery CreateVpcConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcConnectionResponse' smart constructor.
data CreateVpcConnectionResponse = CreateVpcConnectionResponse'
  { -- | The authentication type of VPC connection.
    authentication :: Prelude.Maybe Prelude.Text,
    -- | The list of client subnets.
    clientSubnets :: Prelude.Maybe [Prelude.Text],
    -- | The creation time of VPC connection.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The list of security groups.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The State of Vpc Connection.
    state :: Prelude.Maybe VpcConnectionState,
    -- | A map of tags for the VPC connection.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The VPC connection ARN.
    vpcConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID of the VPC connection.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authentication', 'createVpcConnectionResponse_authentication' - The authentication type of VPC connection.
--
-- 'clientSubnets', 'createVpcConnectionResponse_clientSubnets' - The list of client subnets.
--
-- 'creationTime', 'createVpcConnectionResponse_creationTime' - The creation time of VPC connection.
--
-- 'securityGroups', 'createVpcConnectionResponse_securityGroups' - The list of security groups.
--
-- 'state', 'createVpcConnectionResponse_state' - The State of Vpc Connection.
--
-- 'tags', 'createVpcConnectionResponse_tags' - A map of tags for the VPC connection.
--
-- 'vpcConnectionArn', 'createVpcConnectionResponse_vpcConnectionArn' - The VPC connection ARN.
--
-- 'vpcId', 'createVpcConnectionResponse_vpcId' - The VPC ID of the VPC connection.
--
-- 'httpStatus', 'createVpcConnectionResponse_httpStatus' - The response's http status code.
newCreateVpcConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcConnectionResponse
newCreateVpcConnectionResponse pHttpStatus_ =
  CreateVpcConnectionResponse'
    { authentication =
        Prelude.Nothing,
      clientSubnets = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConnectionArn = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication type of VPC connection.
createVpcConnectionResponse_authentication :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe Prelude.Text)
createVpcConnectionResponse_authentication = Lens.lens (\CreateVpcConnectionResponse' {authentication} -> authentication) (\s@CreateVpcConnectionResponse' {} a -> s {authentication = a} :: CreateVpcConnectionResponse)

-- | The list of client subnets.
createVpcConnectionResponse_clientSubnets :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe [Prelude.Text])
createVpcConnectionResponse_clientSubnets = Lens.lens (\CreateVpcConnectionResponse' {clientSubnets} -> clientSubnets) (\s@CreateVpcConnectionResponse' {} a -> s {clientSubnets = a} :: CreateVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The creation time of VPC connection.
createVpcConnectionResponse_creationTime :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe Prelude.UTCTime)
createVpcConnectionResponse_creationTime = Lens.lens (\CreateVpcConnectionResponse' {creationTime} -> creationTime) (\s@CreateVpcConnectionResponse' {} a -> s {creationTime = a} :: CreateVpcConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The list of security groups.
createVpcConnectionResponse_securityGroups :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe [Prelude.Text])
createVpcConnectionResponse_securityGroups = Lens.lens (\CreateVpcConnectionResponse' {securityGroups} -> securityGroups) (\s@CreateVpcConnectionResponse' {} a -> s {securityGroups = a} :: CreateVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The State of Vpc Connection.
createVpcConnectionResponse_state :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe VpcConnectionState)
createVpcConnectionResponse_state = Lens.lens (\CreateVpcConnectionResponse' {state} -> state) (\s@CreateVpcConnectionResponse' {} a -> s {state = a} :: CreateVpcConnectionResponse)

-- | A map of tags for the VPC connection.
createVpcConnectionResponse_tags :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcConnectionResponse_tags = Lens.lens (\CreateVpcConnectionResponse' {tags} -> tags) (\s@CreateVpcConnectionResponse' {} a -> s {tags = a} :: CreateVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The VPC connection ARN.
createVpcConnectionResponse_vpcConnectionArn :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe Prelude.Text)
createVpcConnectionResponse_vpcConnectionArn = Lens.lens (\CreateVpcConnectionResponse' {vpcConnectionArn} -> vpcConnectionArn) (\s@CreateVpcConnectionResponse' {} a -> s {vpcConnectionArn = a} :: CreateVpcConnectionResponse)

-- | The VPC ID of the VPC connection.
createVpcConnectionResponse_vpcId :: Lens.Lens' CreateVpcConnectionResponse (Prelude.Maybe Prelude.Text)
createVpcConnectionResponse_vpcId = Lens.lens (\CreateVpcConnectionResponse' {vpcId} -> vpcId) (\s@CreateVpcConnectionResponse' {} a -> s {vpcId = a} :: CreateVpcConnectionResponse)

-- | The response's http status code.
createVpcConnectionResponse_httpStatus :: Lens.Lens' CreateVpcConnectionResponse Prelude.Int
createVpcConnectionResponse_httpStatus = Lens.lens (\CreateVpcConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateVpcConnectionResponse' {} a -> s {httpStatus = a} :: CreateVpcConnectionResponse)

instance Prelude.NFData CreateVpcConnectionResponse where
  rnf CreateVpcConnectionResponse' {..} =
    Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf clientSubnets
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConnectionArn
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
