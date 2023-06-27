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
-- Module      : Amazonka.Kafka.DescribeVpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of this MSK VPC connection.
module Amazonka.Kafka.DescribeVpcConnection
  ( -- * Creating a Request
    DescribeVpcConnection (..),
    newDescribeVpcConnection,

    -- * Request Lenses
    describeVpcConnection_arn,

    -- * Destructuring the Response
    DescribeVpcConnectionResponse (..),
    newDescribeVpcConnectionResponse,

    -- * Response Lenses
    describeVpcConnectionResponse_authentication,
    describeVpcConnectionResponse_creationTime,
    describeVpcConnectionResponse_securityGroups,
    describeVpcConnectionResponse_state,
    describeVpcConnectionResponse_subnets,
    describeVpcConnectionResponse_tags,
    describeVpcConnectionResponse_targetClusterArn,
    describeVpcConnectionResponse_vpcConnectionArn,
    describeVpcConnectionResponse_vpcId,
    describeVpcConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcConnection' smart constructor.
data DescribeVpcConnection = DescribeVpcConnection'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
    -- connection.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeVpcConnection_arn' - The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
-- connection.
newDescribeVpcConnection ::
  -- | 'arn'
  Prelude.Text ->
  DescribeVpcConnection
newDescribeVpcConnection pArn_ =
  DescribeVpcConnection' {arn = pArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
-- connection.
describeVpcConnection_arn :: Lens.Lens' DescribeVpcConnection Prelude.Text
describeVpcConnection_arn = Lens.lens (\DescribeVpcConnection' {arn} -> arn) (\s@DescribeVpcConnection' {} a -> s {arn = a} :: DescribeVpcConnection)

instance Core.AWSRequest DescribeVpcConnection where
  type
    AWSResponse DescribeVpcConnection =
      DescribeVpcConnectionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcConnectionResponse'
            Prelude.<$> (x Data..?> "authentication")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "securityGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "subnets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "targetClusterArn")
            Prelude.<*> (x Data..?> "vpcConnectionArn")
            Prelude.<*> (x Data..?> "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVpcConnection where
  hashWithSalt _salt DescribeVpcConnection' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribeVpcConnection where
  rnf DescribeVpcConnection' {..} = Prelude.rnf arn

instance Data.ToHeaders DescribeVpcConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVpcConnection where
  toPath DescribeVpcConnection' {..} =
    Prelude.mconcat
      ["/v1/vpc-connection/", Data.toBS arn]

instance Data.ToQuery DescribeVpcConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVpcConnectionResponse' smart constructor.
data DescribeVpcConnectionResponse = DescribeVpcConnectionResponse'
  { -- | The authentication type of VPC connection.
    authentication :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the VPC connection.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The list of security groups for the VPC connection.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The state of VPC connection.
    state :: Prelude.Maybe VpcConnectionState,
    -- | The list of subnets for the VPC connection.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | A map of tags for the VPC connection.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK cluster.
    targetClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
    -- connection.
    vpcConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The VPC Id for the VPC connection.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authentication', 'describeVpcConnectionResponse_authentication' - The authentication type of VPC connection.
--
-- 'creationTime', 'describeVpcConnectionResponse_creationTime' - The creation time of the VPC connection.
--
-- 'securityGroups', 'describeVpcConnectionResponse_securityGroups' - The list of security groups for the VPC connection.
--
-- 'state', 'describeVpcConnectionResponse_state' - The state of VPC connection.
--
-- 'subnets', 'describeVpcConnectionResponse_subnets' - The list of subnets for the VPC connection.
--
-- 'tags', 'describeVpcConnectionResponse_tags' - A map of tags for the VPC connection.
--
-- 'targetClusterArn', 'describeVpcConnectionResponse_targetClusterArn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK cluster.
--
-- 'vpcConnectionArn', 'describeVpcConnectionResponse_vpcConnectionArn' - The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
-- connection.
--
-- 'vpcId', 'describeVpcConnectionResponse_vpcId' - The VPC Id for the VPC connection.
--
-- 'httpStatus', 'describeVpcConnectionResponse_httpStatus' - The response's http status code.
newDescribeVpcConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcConnectionResponse
newDescribeVpcConnectionResponse pHttpStatus_ =
  DescribeVpcConnectionResponse'
    { authentication =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      state = Prelude.Nothing,
      subnets = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetClusterArn = Prelude.Nothing,
      vpcConnectionArn = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authentication type of VPC connection.
describeVpcConnectionResponse_authentication :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe Prelude.Text)
describeVpcConnectionResponse_authentication = Lens.lens (\DescribeVpcConnectionResponse' {authentication} -> authentication) (\s@DescribeVpcConnectionResponse' {} a -> s {authentication = a} :: DescribeVpcConnectionResponse)

-- | The creation time of the VPC connection.
describeVpcConnectionResponse_creationTime :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe Prelude.UTCTime)
describeVpcConnectionResponse_creationTime = Lens.lens (\DescribeVpcConnectionResponse' {creationTime} -> creationTime) (\s@DescribeVpcConnectionResponse' {} a -> s {creationTime = a} :: DescribeVpcConnectionResponse) Prelude.. Lens.mapping Data._Time

-- | The list of security groups for the VPC connection.
describeVpcConnectionResponse_securityGroups :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe [Prelude.Text])
describeVpcConnectionResponse_securityGroups = Lens.lens (\DescribeVpcConnectionResponse' {securityGroups} -> securityGroups) (\s@DescribeVpcConnectionResponse' {} a -> s {securityGroups = a} :: DescribeVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The state of VPC connection.
describeVpcConnectionResponse_state :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe VpcConnectionState)
describeVpcConnectionResponse_state = Lens.lens (\DescribeVpcConnectionResponse' {state} -> state) (\s@DescribeVpcConnectionResponse' {} a -> s {state = a} :: DescribeVpcConnectionResponse)

-- | The list of subnets for the VPC connection.
describeVpcConnectionResponse_subnets :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe [Prelude.Text])
describeVpcConnectionResponse_subnets = Lens.lens (\DescribeVpcConnectionResponse' {subnets} -> subnets) (\s@DescribeVpcConnectionResponse' {} a -> s {subnets = a} :: DescribeVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A map of tags for the VPC connection.
describeVpcConnectionResponse_tags :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeVpcConnectionResponse_tags = Lens.lens (\DescribeVpcConnectionResponse' {tags} -> tags) (\s@DescribeVpcConnectionResponse' {} a -> s {tags = a} :: DescribeVpcConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK cluster.
describeVpcConnectionResponse_targetClusterArn :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe Prelude.Text)
describeVpcConnectionResponse_targetClusterArn = Lens.lens (\DescribeVpcConnectionResponse' {targetClusterArn} -> targetClusterArn) (\s@DescribeVpcConnectionResponse' {} a -> s {targetClusterArn = a} :: DescribeVpcConnectionResponse)

-- | The Amazon Resource Name (ARN) that uniquely identifies a MSK VPC
-- connection.
describeVpcConnectionResponse_vpcConnectionArn :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe Prelude.Text)
describeVpcConnectionResponse_vpcConnectionArn = Lens.lens (\DescribeVpcConnectionResponse' {vpcConnectionArn} -> vpcConnectionArn) (\s@DescribeVpcConnectionResponse' {} a -> s {vpcConnectionArn = a} :: DescribeVpcConnectionResponse)

-- | The VPC Id for the VPC connection.
describeVpcConnectionResponse_vpcId :: Lens.Lens' DescribeVpcConnectionResponse (Prelude.Maybe Prelude.Text)
describeVpcConnectionResponse_vpcId = Lens.lens (\DescribeVpcConnectionResponse' {vpcId} -> vpcId) (\s@DescribeVpcConnectionResponse' {} a -> s {vpcId = a} :: DescribeVpcConnectionResponse)

-- | The response's http status code.
describeVpcConnectionResponse_httpStatus :: Lens.Lens' DescribeVpcConnectionResponse Prelude.Int
describeVpcConnectionResponse_httpStatus = Lens.lens (\DescribeVpcConnectionResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcConnectionResponse' {} a -> s {httpStatus = a} :: DescribeVpcConnectionResponse)

instance Prelude.NFData DescribeVpcConnectionResponse where
  rnf DescribeVpcConnectionResponse' {..} =
    Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetClusterArn
      `Prelude.seq` Prelude.rnf vpcConnectionArn
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
