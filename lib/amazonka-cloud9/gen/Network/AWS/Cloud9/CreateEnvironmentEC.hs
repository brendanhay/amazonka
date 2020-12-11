{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ceecAutomaticStopTimeMinutes,
    ceecSubnetId,
    ceecOwnerARN,
    ceecClientRequestToken,
    ceecConnectionType,
    ceecDescription,
    ceecTags,
    ceecName,
    ceecInstanceType,

    -- * Destructuring the response
    CreateEnvironmentECResponse (..),
    mkCreateEnvironmentECResponse,

    -- ** Response lenses
    ceecrsEnvironmentId,
    ceecrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEnvironmentEC' smart constructor.
data CreateEnvironmentEC = CreateEnvironmentEC'
  { automaticStopTimeMinutes ::
      Lude.Maybe Lude.Int,
    subnetId :: Lude.Maybe Lude.Text,
    ownerARN :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    connectionType :: Lude.Maybe ConnectionType,
    description ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    instanceType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEnvironmentEC' with the minimum fields required to make a request.
--
-- * 'automaticStopTimeMinutes' - The number of minutes until the running instance is shut down after the environment has last been used.
-- * 'clientRequestToken' - A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
-- * 'connectionType' - The connection type used for connecting to an Amazon EC2 environment.
-- * 'description' - The description of the environment to create.
-- * 'instanceType' - The type of instance to connect to the environment (for example, @t2.micro@ ).
-- * 'name' - The name of the environment to create.
--
-- This name is visible to other AWS IAM users in the same AWS account.
-- * 'ownerARN' - The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
-- * 'subnetId' - The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
-- * 'tags' - An array of key-value pairs that will be associated with the new AWS Cloud9 development environment.
mkCreateEnvironmentEC ::
  -- | 'name'
  Lude.Text ->
  -- | 'instanceType'
  Lude.Text ->
  CreateEnvironmentEC
mkCreateEnvironmentEC pName_ pInstanceType_ =
  CreateEnvironmentEC'
    { automaticStopTimeMinutes = Lude.Nothing,
      subnetId = Lude.Nothing,
      ownerARN = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      connectionType = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

-- | The number of minutes until the running instance is shut down after the environment has last been used.
--
-- /Note:/ Consider using 'automaticStopTimeMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecAutomaticStopTimeMinutes :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe Lude.Int)
ceecAutomaticStopTimeMinutes = Lens.lens (automaticStopTimeMinutes :: CreateEnvironmentEC -> Lude.Maybe Lude.Int) (\s a -> s {automaticStopTimeMinutes = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecAutomaticStopTimeMinutes "Use generic-lens or generic-optics with 'automaticStopTimeMinutes' instead." #-}

-- | The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecSubnetId :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe Lude.Text)
ceecSubnetId = Lens.lens (subnetId :: CreateEnvironmentEC -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
--
-- /Note:/ Consider using 'ownerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecOwnerARN :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe Lude.Text)
ceecOwnerARN = Lens.lens (ownerARN :: CreateEnvironmentEC -> Lude.Maybe Lude.Text) (\s a -> s {ownerARN = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecOwnerARN "Use generic-lens or generic-optics with 'ownerARN' instead." #-}

-- | A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecClientRequestToken :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe Lude.Text)
ceecClientRequestToken = Lens.lens (clientRequestToken :: CreateEnvironmentEC -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The connection type used for connecting to an Amazon EC2 environment.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecConnectionType :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe ConnectionType)
ceecConnectionType = Lens.lens (connectionType :: CreateEnvironmentEC -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | The description of the environment to create.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecDescription :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe (Lude.Sensitive Lude.Text))
ceecDescription = Lens.lens (description :: CreateEnvironmentEC -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An array of key-value pairs that will be associated with the new AWS Cloud9 development environment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecTags :: Lens.Lens' CreateEnvironmentEC (Lude.Maybe [Tag])
ceecTags = Lens.lens (tags :: CreateEnvironmentEC -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the environment to create.
--
-- This name is visible to other AWS IAM users in the same AWS account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecName :: Lens.Lens' CreateEnvironmentEC Lude.Text
ceecName = Lens.lens (name :: CreateEnvironmentEC -> Lude.Text) (\s a -> s {name = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of instance to connect to the environment (for example, @t2.micro@ ).
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecInstanceType :: Lens.Lens' CreateEnvironmentEC Lude.Text
ceecInstanceType = Lens.lens (instanceType :: CreateEnvironmentEC -> Lude.Text) (\s a -> s {instanceType = a} :: CreateEnvironmentEC)
{-# DEPRECATED ceecInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

instance Lude.AWSRequest CreateEnvironmentEC where
  type Rs CreateEnvironmentEC = CreateEnvironmentECResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEnvironmentECResponse'
            Lude.<$> (x Lude..?> "environmentId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEnvironmentEC where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.CreateEnvironmentEC" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEnvironmentEC where
  toJSON CreateEnvironmentEC' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("automaticStopTimeMinutes" Lude..=)
              Lude.<$> automaticStopTimeMinutes,
            ("subnetId" Lude..=) Lude.<$> subnetId,
            ("ownerArn" Lude..=) Lude.<$> ownerARN,
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("connectionType" Lude..=) Lude.<$> connectionType,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("instanceType" Lude..= instanceType)
          ]
      )

instance Lude.ToPath CreateEnvironmentEC where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEnvironmentEC where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEnvironmentECResponse' smart constructor.
data CreateEnvironmentECResponse = CreateEnvironmentECResponse'
  { environmentId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEnvironmentECResponse' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment that was created.
-- * 'responseStatus' - The response status code.
mkCreateEnvironmentECResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEnvironmentECResponse
mkCreateEnvironmentECResponse pResponseStatus_ =
  CreateEnvironmentECResponse'
    { environmentId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the environment that was created.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecrsEnvironmentId :: Lens.Lens' CreateEnvironmentECResponse (Lude.Maybe Lude.Text)
ceecrsEnvironmentId = Lens.lens (environmentId :: CreateEnvironmentECResponse -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: CreateEnvironmentECResponse)
{-# DEPRECATED ceecrsEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceecrsResponseStatus :: Lens.Lens' CreateEnvironmentECResponse Lude.Int
ceecrsResponseStatus = Lens.lens (responseStatus :: CreateEnvironmentECResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEnvironmentECResponse)
{-# DEPRECATED ceecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
