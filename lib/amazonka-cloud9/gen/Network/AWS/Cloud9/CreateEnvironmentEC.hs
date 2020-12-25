{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.CreateEnvironmentEC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Cloud9 development environment, launches an Amazon Elastic Compute Cloud (Amazon EC2) instance, and then connects from the instance to the environment.
module Network.AWS.Cloud9.CreateEnvironmentEC
  ( -- * Creating a request
    CreateEnvironmentEC (..),
    mkCreateEnvironmentEC,

    -- ** Request lenses
    ceecName,
    ceecInstanceType,
    ceecAutomaticStopTimeMinutes,
    ceecClientRequestToken,
    ceecConnectionType,
    ceecDescription,
    ceecOwnerArn,
    ceecSubnetId,
    ceecTags,

    -- * Destructuring the response
    CreateEnvironmentECResponse (..),
    mkCreateEnvironmentECResponse,

    -- ** Response lenses
    ceecrrsEnvironmentId,
    ceecrrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEnvironmentEC' smart constructor.
data CreateEnvironmentEC = CreateEnvironmentEC'
  { -- | The name of the environment to create.
    --
    -- This name is visible to other AWS IAM users in the same AWS account.
    name :: Types.Name,
    -- | The type of instance to connect to the environment (for example, @t2.micro@ ).
    instanceType :: Types.InstanceType,
    -- | The number of minutes until the running instance is shut down after the environment has last been used.
    automaticStopTimeMinutes :: Core.Maybe Core.Int,
    -- | A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time.
    --
    -- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    connectionType :: Core.Maybe Types.ConnectionType,
    -- | The description of the environment to create.
    description :: Core.Maybe Types.Description,
    -- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
    ownerArn :: Core.Maybe Types.OwnerArn,
    -- | The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | An array of key-value pairs that will be associated with the new AWS Cloud9 development environment.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEnvironmentEC' value with any optional fields omitted.
mkCreateEnvironmentEC ::
  -- | 'name'
  Types.Name ->
  -- | 'instanceType'
  Types.InstanceType ->
  CreateEnvironmentEC
mkCreateEnvironmentEC name instanceType =
  CreateEnvironmentEC'
    { name,
      instanceType,
      automaticStopTimeMinutes = Core.Nothing,
      clientRequestToken = Core.Nothing,
      connectionType = Core.Nothing,
      description = Core.Nothing,
      ownerArn = Core.Nothing,
      subnetId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the environment to create.
--
-- This name is visible to other AWS IAM users in the same AWS account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecName :: Lens.Lens' CreateEnvironmentEC Types.Name
ceecName = Lens.field @"name"
{-# DEPRECATED ceecName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of instance to connect to the environment (for example, @t2.micro@ ).
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecInstanceType :: Lens.Lens' CreateEnvironmentEC Types.InstanceType
ceecInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ceecInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of minutes until the running instance is shut down after the environment has last been used.
--
-- /Note:/ Consider using 'automaticStopTimeMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecAutomaticStopTimeMinutes :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Core.Int)
ceecAutomaticStopTimeMinutes = Lens.field @"automaticStopTimeMinutes"
{-# DEPRECATED ceecAutomaticStopTimeMinutes "Use generic-lens or generic-optics with 'automaticStopTimeMinutes' instead." #-}

-- | A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecClientRequestToken :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Types.ClientRequestToken)
ceecClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED ceecClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The connection type used for connecting to an Amazon EC2 environment.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecConnectionType :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Types.ConnectionType)
ceecConnectionType = Lens.field @"connectionType"
{-# DEPRECATED ceecConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | The description of the environment to create.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecDescription :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Types.Description)
ceecDescription = Lens.field @"description"
{-# DEPRECATED ceecDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
--
-- /Note:/ Consider using 'ownerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecOwnerArn :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Types.OwnerArn)
ceecOwnerArn = Lens.field @"ownerArn"
{-# DEPRECATED ceecOwnerArn "Use generic-lens or generic-optics with 'ownerArn' instead." #-}

-- | The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecSubnetId :: Lens.Lens' CreateEnvironmentEC (Core.Maybe Types.SubnetId)
ceecSubnetId = Lens.field @"subnetId"
{-# DEPRECATED ceecSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | An array of key-value pairs that will be associated with the new AWS Cloud9 development environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecTags :: Lens.Lens' CreateEnvironmentEC (Core.Maybe [Types.Tag])
ceecTags = Lens.field @"tags"
{-# DEPRECATED ceecTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateEnvironmentEC where
  toJSON CreateEnvironmentEC {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("instanceType" Core..= instanceType),
            ("automaticStopTimeMinutes" Core..=)
              Core.<$> automaticStopTimeMinutes,
            ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("connectionType" Core..=) Core.<$> connectionType,
            ("description" Core..=) Core.<$> description,
            ("ownerArn" Core..=) Core.<$> ownerArn,
            ("subnetId" Core..=) Core.<$> subnetId,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateEnvironmentEC where
  type Rs CreateEnvironmentEC = CreateEnvironmentECResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.CreateEnvironmentEC"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentECResponse'
            Core.<$> (x Core..:? "environmentId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEnvironmentECResponse' smart constructor.
data CreateEnvironmentECResponse = CreateEnvironmentECResponse'
  { -- | The ID of the environment that was created.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEnvironmentECResponse' value with any optional fields omitted.
mkCreateEnvironmentECResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEnvironmentECResponse
mkCreateEnvironmentECResponse responseStatus =
  CreateEnvironmentECResponse'
    { environmentId = Core.Nothing,
      responseStatus
    }

-- | The ID of the environment that was created.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecrrsEnvironmentId :: Lens.Lens' CreateEnvironmentECResponse (Core.Maybe Types.EnvironmentId)
ceecrrsEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED ceecrrsEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecrrsResponseStatus :: Lens.Lens' CreateEnvironmentECResponse Core.Int
ceecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ceecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
