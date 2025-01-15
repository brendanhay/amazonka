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
-- Module      : Amazonka.Kafka.CreateClusterV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MSK cluster.
module Amazonka.Kafka.CreateClusterV2
  ( -- * Creating a Request
    CreateClusterV2 (..),
    newCreateClusterV2,

    -- * Request Lenses
    createClusterV2_provisioned,
    createClusterV2_serverless,
    createClusterV2_tags,
    createClusterV2_clusterName,

    -- * Destructuring the Response
    CreateClusterV2Response (..),
    newCreateClusterV2Response,

    -- * Response Lenses
    createClusterV2Response_clusterArn,
    createClusterV2Response_clusterName,
    createClusterV2Response_clusterType,
    createClusterV2Response_state,
    createClusterV2Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateClusterV2' smart constructor.
data CreateClusterV2 = CreateClusterV2'
  { -- | Information about the provisioned cluster.
    provisioned :: Prelude.Maybe ProvisionedRequest,
    -- | Information about the serverless cluster.
    serverless :: Prelude.Maybe ServerlessRequest,
    -- | A map of tags that you want the cluster to have.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cluster.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioned', 'createClusterV2_provisioned' - Information about the provisioned cluster.
--
-- 'serverless', 'createClusterV2_serverless' - Information about the serverless cluster.
--
-- 'tags', 'createClusterV2_tags' - A map of tags that you want the cluster to have.
--
-- 'clusterName', 'createClusterV2_clusterName' - The name of the cluster.
newCreateClusterV2 ::
  -- | 'clusterName'
  Prelude.Text ->
  CreateClusterV2
newCreateClusterV2 pClusterName_ =
  CreateClusterV2'
    { provisioned = Prelude.Nothing,
      serverless = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | Information about the provisioned cluster.
createClusterV2_provisioned :: Lens.Lens' CreateClusterV2 (Prelude.Maybe ProvisionedRequest)
createClusterV2_provisioned = Lens.lens (\CreateClusterV2' {provisioned} -> provisioned) (\s@CreateClusterV2' {} a -> s {provisioned = a} :: CreateClusterV2)

-- | Information about the serverless cluster.
createClusterV2_serverless :: Lens.Lens' CreateClusterV2 (Prelude.Maybe ServerlessRequest)
createClusterV2_serverless = Lens.lens (\CreateClusterV2' {serverless} -> serverless) (\s@CreateClusterV2' {} a -> s {serverless = a} :: CreateClusterV2)

-- | A map of tags that you want the cluster to have.
createClusterV2_tags :: Lens.Lens' CreateClusterV2 (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createClusterV2_tags = Lens.lens (\CreateClusterV2' {tags} -> tags) (\s@CreateClusterV2' {} a -> s {tags = a} :: CreateClusterV2) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster.
createClusterV2_clusterName :: Lens.Lens' CreateClusterV2 Prelude.Text
createClusterV2_clusterName = Lens.lens (\CreateClusterV2' {clusterName} -> clusterName) (\s@CreateClusterV2' {} a -> s {clusterName = a} :: CreateClusterV2)

instance Core.AWSRequest CreateClusterV2 where
  type
    AWSResponse CreateClusterV2 =
      CreateClusterV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterV2Response'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterName")
            Prelude.<*> (x Data..?> "clusterType")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClusterV2 where
  hashWithSalt _salt CreateClusterV2' {..} =
    _salt
      `Prelude.hashWithSalt` provisioned
      `Prelude.hashWithSalt` serverless
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData CreateClusterV2 where
  rnf CreateClusterV2' {..} =
    Prelude.rnf provisioned `Prelude.seq`
      Prelude.rnf serverless `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf clusterName

instance Data.ToHeaders CreateClusterV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateClusterV2 where
  toJSON CreateClusterV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("provisioned" Data..=) Prelude.<$> provisioned,
            ("serverless" Data..=) Prelude.<$> serverless,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("clusterName" Data..= clusterName)
          ]
      )

instance Data.ToPath CreateClusterV2 where
  toPath = Prelude.const "/api/v2/clusters"

instance Data.ToQuery CreateClusterV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterV2Response' smart constructor.
data CreateClusterV2Response = CreateClusterV2Response'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the MSK cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The type of the cluster. The possible states are PROVISIONED or
    -- SERVERLESS.
    clusterType :: Prelude.Maybe ClusterType,
    -- | The state of the cluster. The possible states are ACTIVE, CREATING,
    -- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
    state :: Prelude.Maybe ClusterState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'createClusterV2Response_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterName', 'createClusterV2Response_clusterName' - The name of the MSK cluster.
--
-- 'clusterType', 'createClusterV2Response_clusterType' - The type of the cluster. The possible states are PROVISIONED or
-- SERVERLESS.
--
-- 'state', 'createClusterV2Response_state' - The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
--
-- 'httpStatus', 'createClusterV2Response_httpStatus' - The response's http status code.
newCreateClusterV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterV2Response
newCreateClusterV2Response pHttpStatus_ =
  CreateClusterV2Response'
    { clusterArn =
        Prelude.Nothing,
      clusterName = Prelude.Nothing,
      clusterType = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
createClusterV2Response_clusterArn :: Lens.Lens' CreateClusterV2Response (Prelude.Maybe Prelude.Text)
createClusterV2Response_clusterArn = Lens.lens (\CreateClusterV2Response' {clusterArn} -> clusterArn) (\s@CreateClusterV2Response' {} a -> s {clusterArn = a} :: CreateClusterV2Response)

-- | The name of the MSK cluster.
createClusterV2Response_clusterName :: Lens.Lens' CreateClusterV2Response (Prelude.Maybe Prelude.Text)
createClusterV2Response_clusterName = Lens.lens (\CreateClusterV2Response' {clusterName} -> clusterName) (\s@CreateClusterV2Response' {} a -> s {clusterName = a} :: CreateClusterV2Response)

-- | The type of the cluster. The possible states are PROVISIONED or
-- SERVERLESS.
createClusterV2Response_clusterType :: Lens.Lens' CreateClusterV2Response (Prelude.Maybe ClusterType)
createClusterV2Response_clusterType = Lens.lens (\CreateClusterV2Response' {clusterType} -> clusterType) (\s@CreateClusterV2Response' {} a -> s {clusterType = a} :: CreateClusterV2Response)

-- | The state of the cluster. The possible states are ACTIVE, CREATING,
-- DELETING, FAILED, HEALING, MAINTENANCE, REBOOTING_BROKER, and UPDATING.
createClusterV2Response_state :: Lens.Lens' CreateClusterV2Response (Prelude.Maybe ClusterState)
createClusterV2Response_state = Lens.lens (\CreateClusterV2Response' {state} -> state) (\s@CreateClusterV2Response' {} a -> s {state = a} :: CreateClusterV2Response)

-- | The response's http status code.
createClusterV2Response_httpStatus :: Lens.Lens' CreateClusterV2Response Prelude.Int
createClusterV2Response_httpStatus = Lens.lens (\CreateClusterV2Response' {httpStatus} -> httpStatus) (\s@CreateClusterV2Response' {} a -> s {httpStatus = a} :: CreateClusterV2Response)

instance Prelude.NFData CreateClusterV2Response where
  rnf CreateClusterV2Response' {..} =
    Prelude.rnf clusterArn `Prelude.seq`
      Prelude.rnf clusterName `Prelude.seq`
        Prelude.rnf clusterType `Prelude.seq`
          Prelude.rnf state `Prelude.seq`
            Prelude.rnf httpStatus
