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
-- Module      : Network.AWS.Cloud9.CreateEnvironmentEC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Cloud9 development environment, launches an Amazon Elastic
-- Compute Cloud (Amazon EC2) instance, and then connects from the instance
-- to the environment.
module Network.AWS.Cloud9.CreateEnvironmentEC
  ( -- * Creating a Request
    CreateEnvironmentEC (..),
    newCreateEnvironmentEC,

    -- * Request Lenses
    createEnvironmentEC_dryRun,
    createEnvironmentEC_connectionType,
    createEnvironmentEC_imageId,
    createEnvironmentEC_ownerArn,
    createEnvironmentEC_tags,
    createEnvironmentEC_subnetId,
    createEnvironmentEC_description,
    createEnvironmentEC_clientRequestToken,
    createEnvironmentEC_automaticStopTimeMinutes,
    createEnvironmentEC_name,
    createEnvironmentEC_instanceType,

    -- * Destructuring the Response
    CreateEnvironmentECResponse (..),
    newCreateEnvironmentECResponse,

    -- * Response Lenses
    createEnvironmentECResponse_environmentId,
    createEnvironmentECResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEnvironmentEC' smart constructor.
data CreateEnvironmentEC = CreateEnvironmentEC'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    -- Valid values are @CONNECT_SSH@ (default) and @CONNECT_SSM@ (connected
    -- through Amazon EC2 Systems Manager).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloud9/latest/user-guide/ec2-ssm.html Accessing no-ingress EC2 instances with Amazon EC2 Systems Manager>
    -- in the /Cloud9 User Guide/.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | The identifier for the Amazon Machine Image (AMI) that\'s used to create
    -- the EC2 instance. To choose an AMI for the instance, you must specify a
    -- valid AMI alias or a valid Amazon EC2 Systems Manager (SSM) path.
    --
    -- The default AMI is used if the parameter isn\'t explicitly assigned a
    -- value in the request. Because Amazon Linux AMI has ended standard
    -- support as of December 31, 2020, we recommend you choose Amazon Linux 2,
    -- which includes long term support through 2023.
    --
    -- __AMI aliases__
    --
    -- -   __Amazon Linux (default): @amazonlinux-1-x86_64@__
    --
    -- -   Amazon Linux 2: @amazonlinux-2-x86_64@
    --
    -- -   Ubuntu 18.04: @ubuntu-18.04-x86_64@
    --
    -- __SSM paths__
    --
    -- -   __Amazon Linux (default):
    --     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-1-x86_64@__
    --
    -- -   Amazon Linux 2:
    --     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-2-x86_64@
    --
    -- -   Ubuntu 18.04:
    --     @resolve:ssm:\/aws\/service\/cloud9\/amis\/ubuntu-18.04-x86_64@
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be
    -- the ARN of any IAM principal. If this value is not specified, the ARN
    -- defaults to this environment\'s creator.
    ownerArn :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs that will be associated with the new Cloud9
    -- development environment.
    tags :: Prelude.Maybe (Core.Sensitive [Core.Sensitive Tag]),
    -- | The ID of the subnet in Amazon VPC that Cloud9 will use to communicate
    -- with the Amazon EC2 instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment to create.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A unique, case-sensitive string that helps Cloud9 to ensure this
    -- operation completes no more than one time.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens>
    -- in the /Amazon EC2 API Reference/.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes until the running instance is shut down after the
    -- environment has last been used.
    automaticStopTimeMinutes :: Prelude.Maybe Prelude.Int,
    -- | The name of the environment to create.
    --
    -- This name is visible to other IAM users in the same Amazon Web Services
    -- account.
    name :: Prelude.Text,
    -- | The type of instance to connect to the environment (for example,
    -- @t2.micro@).
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentEC' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createEnvironmentEC_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionType', 'createEnvironmentEC_connectionType' - The connection type used for connecting to an Amazon EC2 environment.
-- Valid values are @CONNECT_SSH@ (default) and @CONNECT_SSM@ (connected
-- through Amazon EC2 Systems Manager).
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloud9/latest/user-guide/ec2-ssm.html Accessing no-ingress EC2 instances with Amazon EC2 Systems Manager>
-- in the /Cloud9 User Guide/.
--
-- 'imageId', 'createEnvironmentEC_imageId' - The identifier for the Amazon Machine Image (AMI) that\'s used to create
-- the EC2 instance. To choose an AMI for the instance, you must specify a
-- valid AMI alias or a valid Amazon EC2 Systems Manager (SSM) path.
--
-- The default AMI is used if the parameter isn\'t explicitly assigned a
-- value in the request. Because Amazon Linux AMI has ended standard
-- support as of December 31, 2020, we recommend you choose Amazon Linux 2,
-- which includes long term support through 2023.
--
-- __AMI aliases__
--
-- -   __Amazon Linux (default): @amazonlinux-1-x86_64@__
--
-- -   Amazon Linux 2: @amazonlinux-2-x86_64@
--
-- -   Ubuntu 18.04: @ubuntu-18.04-x86_64@
--
-- __SSM paths__
--
-- -   __Amazon Linux (default):
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-1-x86_64@__
--
-- -   Amazon Linux 2:
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-2-x86_64@
--
-- -   Ubuntu 18.04:
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/ubuntu-18.04-x86_64@
--
-- 'ownerArn', 'createEnvironmentEC_ownerArn' - The Amazon Resource Name (ARN) of the environment owner. This ARN can be
-- the ARN of any IAM principal. If this value is not specified, the ARN
-- defaults to this environment\'s creator.
--
-- 'tags', 'createEnvironmentEC_tags' - An array of key-value pairs that will be associated with the new Cloud9
-- development environment.
--
-- 'subnetId', 'createEnvironmentEC_subnetId' - The ID of the subnet in Amazon VPC that Cloud9 will use to communicate
-- with the Amazon EC2 instance.
--
-- 'description', 'createEnvironmentEC_description' - The description of the environment to create.
--
-- 'clientRequestToken', 'createEnvironmentEC_clientRequestToken' - A unique, case-sensitive string that helps Cloud9 to ensure this
-- operation completes no more than one time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens>
-- in the /Amazon EC2 API Reference/.
--
-- 'automaticStopTimeMinutes', 'createEnvironmentEC_automaticStopTimeMinutes' - The number of minutes until the running instance is shut down after the
-- environment has last been used.
--
-- 'name', 'createEnvironmentEC_name' - The name of the environment to create.
--
-- This name is visible to other IAM users in the same Amazon Web Services
-- account.
--
-- 'instanceType', 'createEnvironmentEC_instanceType' - The type of instance to connect to the environment (for example,
-- @t2.micro@).
newCreateEnvironmentEC ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  CreateEnvironmentEC
newCreateEnvironmentEC pName_ pInstanceType_ =
  CreateEnvironmentEC'
    { dryRun = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      imageId = Prelude.Nothing,
      ownerArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      automaticStopTimeMinutes = Prelude.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createEnvironmentEC_dryRun :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Bool)
createEnvironmentEC_dryRun = Lens.lens (\CreateEnvironmentEC' {dryRun} -> dryRun) (\s@CreateEnvironmentEC' {} a -> s {dryRun = a} :: CreateEnvironmentEC)

-- | The connection type used for connecting to an Amazon EC2 environment.
-- Valid values are @CONNECT_SSH@ (default) and @CONNECT_SSM@ (connected
-- through Amazon EC2 Systems Manager).
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloud9/latest/user-guide/ec2-ssm.html Accessing no-ingress EC2 instances with Amazon EC2 Systems Manager>
-- in the /Cloud9 User Guide/.
createEnvironmentEC_connectionType :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe ConnectionType)
createEnvironmentEC_connectionType = Lens.lens (\CreateEnvironmentEC' {connectionType} -> connectionType) (\s@CreateEnvironmentEC' {} a -> s {connectionType = a} :: CreateEnvironmentEC)

-- | The identifier for the Amazon Machine Image (AMI) that\'s used to create
-- the EC2 instance. To choose an AMI for the instance, you must specify a
-- valid AMI alias or a valid Amazon EC2 Systems Manager (SSM) path.
--
-- The default AMI is used if the parameter isn\'t explicitly assigned a
-- value in the request. Because Amazon Linux AMI has ended standard
-- support as of December 31, 2020, we recommend you choose Amazon Linux 2,
-- which includes long term support through 2023.
--
-- __AMI aliases__
--
-- -   __Amazon Linux (default): @amazonlinux-1-x86_64@__
--
-- -   Amazon Linux 2: @amazonlinux-2-x86_64@
--
-- -   Ubuntu 18.04: @ubuntu-18.04-x86_64@
--
-- __SSM paths__
--
-- -   __Amazon Linux (default):
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-1-x86_64@__
--
-- -   Amazon Linux 2:
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/amazonlinux-2-x86_64@
--
-- -   Ubuntu 18.04:
--     @resolve:ssm:\/aws\/service\/cloud9\/amis\/ubuntu-18.04-x86_64@
createEnvironmentEC_imageId :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Text)
createEnvironmentEC_imageId = Lens.lens (\CreateEnvironmentEC' {imageId} -> imageId) (\s@CreateEnvironmentEC' {} a -> s {imageId = a} :: CreateEnvironmentEC)

-- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be
-- the ARN of any IAM principal. If this value is not specified, the ARN
-- defaults to this environment\'s creator.
createEnvironmentEC_ownerArn :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Text)
createEnvironmentEC_ownerArn = Lens.lens (\CreateEnvironmentEC' {ownerArn} -> ownerArn) (\s@CreateEnvironmentEC' {} a -> s {ownerArn = a} :: CreateEnvironmentEC)

-- | An array of key-value pairs that will be associated with the new Cloud9
-- development environment.
createEnvironmentEC_tags :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe [Tag])
createEnvironmentEC_tags = Lens.lens (\CreateEnvironmentEC' {tags} -> tags) (\s@CreateEnvironmentEC' {} a -> s {tags = a} :: CreateEnvironmentEC) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

-- | The ID of the subnet in Amazon VPC that Cloud9 will use to communicate
-- with the Amazon EC2 instance.
createEnvironmentEC_subnetId :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Text)
createEnvironmentEC_subnetId = Lens.lens (\CreateEnvironmentEC' {subnetId} -> subnetId) (\s@CreateEnvironmentEC' {} a -> s {subnetId = a} :: CreateEnvironmentEC)

-- | The description of the environment to create.
createEnvironmentEC_description :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Text)
createEnvironmentEC_description = Lens.lens (\CreateEnvironmentEC' {description} -> description) (\s@CreateEnvironmentEC' {} a -> s {description = a} :: CreateEnvironmentEC) Prelude.. Lens.mapping Core._Sensitive

-- | A unique, case-sensitive string that helps Cloud9 to ensure this
-- operation completes no more than one time.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens>
-- in the /Amazon EC2 API Reference/.
createEnvironmentEC_clientRequestToken :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Text)
createEnvironmentEC_clientRequestToken = Lens.lens (\CreateEnvironmentEC' {clientRequestToken} -> clientRequestToken) (\s@CreateEnvironmentEC' {} a -> s {clientRequestToken = a} :: CreateEnvironmentEC)

-- | The number of minutes until the running instance is shut down after the
-- environment has last been used.
createEnvironmentEC_automaticStopTimeMinutes :: Lens.Lens' CreateEnvironmentEC (Prelude.Maybe Prelude.Int)
createEnvironmentEC_automaticStopTimeMinutes = Lens.lens (\CreateEnvironmentEC' {automaticStopTimeMinutes} -> automaticStopTimeMinutes) (\s@CreateEnvironmentEC' {} a -> s {automaticStopTimeMinutes = a} :: CreateEnvironmentEC)

-- | The name of the environment to create.
--
-- This name is visible to other IAM users in the same Amazon Web Services
-- account.
createEnvironmentEC_name :: Lens.Lens' CreateEnvironmentEC Prelude.Text
createEnvironmentEC_name = Lens.lens (\CreateEnvironmentEC' {name} -> name) (\s@CreateEnvironmentEC' {} a -> s {name = a} :: CreateEnvironmentEC)

-- | The type of instance to connect to the environment (for example,
-- @t2.micro@).
createEnvironmentEC_instanceType :: Lens.Lens' CreateEnvironmentEC Prelude.Text
createEnvironmentEC_instanceType = Lens.lens (\CreateEnvironmentEC' {instanceType} -> instanceType) (\s@CreateEnvironmentEC' {} a -> s {instanceType = a} :: CreateEnvironmentEC)

instance Core.AWSRequest CreateEnvironmentEC where
  type
    AWSResponse CreateEnvironmentEC =
      CreateEnvironmentECResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentECResponse'
            Prelude.<$> (x Core..?> "environmentId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEnvironmentEC

instance Prelude.NFData CreateEnvironmentEC

instance Core.ToHeaders CreateEnvironmentEC where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.CreateEnvironmentEC" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEnvironmentEC where
  toJSON CreateEnvironmentEC' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dryRun" Core..=) Prelude.<$> dryRun,
            ("connectionType" Core..=)
              Prelude.<$> connectionType,
            ("imageId" Core..=) Prelude.<$> imageId,
            ("ownerArn" Core..=) Prelude.<$> ownerArn,
            ("tags" Core..=) Prelude.<$> tags,
            ("subnetId" Core..=) Prelude.<$> subnetId,
            ("description" Core..=) Prelude.<$> description,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("automaticStopTimeMinutes" Core..=)
              Prelude.<$> automaticStopTimeMinutes,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("instanceType" Core..= instanceType)
          ]
      )

instance Core.ToPath CreateEnvironmentEC where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEnvironmentEC where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentECResponse' smart constructor.
data CreateEnvironmentECResponse = CreateEnvironmentECResponse'
  { -- | The ID of the environment that was created.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentECResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'createEnvironmentECResponse_environmentId' - The ID of the environment that was created.
--
-- 'httpStatus', 'createEnvironmentECResponse_httpStatus' - The response's http status code.
newCreateEnvironmentECResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEnvironmentECResponse
newCreateEnvironmentECResponse pHttpStatus_ =
  CreateEnvironmentECResponse'
    { environmentId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the environment that was created.
createEnvironmentECResponse_environmentId :: Lens.Lens' CreateEnvironmentECResponse (Prelude.Maybe Prelude.Text)
createEnvironmentECResponse_environmentId = Lens.lens (\CreateEnvironmentECResponse' {environmentId} -> environmentId) (\s@CreateEnvironmentECResponse' {} a -> s {environmentId = a} :: CreateEnvironmentECResponse)

-- | The response's http status code.
createEnvironmentECResponse_httpStatus :: Lens.Lens' CreateEnvironmentECResponse Prelude.Int
createEnvironmentECResponse_httpStatus = Lens.lens (\CreateEnvironmentECResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentECResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentECResponse)

instance Prelude.NFData CreateEnvironmentECResponse
