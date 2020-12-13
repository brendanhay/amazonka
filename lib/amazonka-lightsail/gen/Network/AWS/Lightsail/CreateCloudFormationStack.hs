{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateCloudFormationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation stack, which creates a new Amazon EC2 instance from an exported Amazon Lightsail snapshot. This operation results in a CloudFormation stack record that can be used to track the AWS CloudFormation stack created. Use the @get cloud formation stack records@ operation to get a list of the CloudFormation stacks created.
--
-- /Important:/ Wait until after your new Amazon EC2 instance is created before running the @create cloud formation stack@ operation again with the same export snapshot record.
module Network.AWS.Lightsail.CreateCloudFormationStack
  ( -- * Creating a request
    CreateCloudFormationStack (..),
    mkCreateCloudFormationStack,

    -- ** Request lenses
    ccfsInstances,

    -- * Destructuring the response
    CreateCloudFormationStackResponse (..),
    mkCreateCloudFormationStackResponse,

    -- ** Response lenses
    ccfsrsOperations,
    ccfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCloudFormationStack' smart constructor.
newtype CreateCloudFormationStack = CreateCloudFormationStack'
  { -- | An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
    instances :: [InstanceEntry]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFormationStack' with the minimum fields required to make a request.
--
-- * 'instances' - An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
mkCreateCloudFormationStack ::
  CreateCloudFormationStack
mkCreateCloudFormationStack =
  CreateCloudFormationStack' {instances = Lude.mempty}

-- | An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsInstances :: Lens.Lens' CreateCloudFormationStack [InstanceEntry]
ccfsInstances = Lens.lens (instances :: CreateCloudFormationStack -> [InstanceEntry]) (\s a -> s {instances = a} :: CreateCloudFormationStack)
{-# DEPRECATED ccfsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.AWSRequest CreateCloudFormationStack where
  type
    Rs CreateCloudFormationStack =
      CreateCloudFormationStackResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCloudFormationStackResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCloudFormationStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateCloudFormationStack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCloudFormationStack where
  toJSON CreateCloudFormationStack' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instances" Lude..= instances)])

instance Lude.ToPath CreateCloudFormationStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCloudFormationStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCloudFormationStackResponse' smart constructor.
data CreateCloudFormationStackResponse = CreateCloudFormationStackResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFormationStackResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateCloudFormationStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCloudFormationStackResponse
mkCreateCloudFormationStackResponse pResponseStatus_ =
  CreateCloudFormationStackResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsrsOperations :: Lens.Lens' CreateCloudFormationStackResponse (Lude.Maybe [Operation])
ccfsrsOperations = Lens.lens (operations :: CreateCloudFormationStackResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateCloudFormationStackResponse)
{-# DEPRECATED ccfsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfsrsResponseStatus :: Lens.Lens' CreateCloudFormationStackResponse Lude.Int
ccfsrsResponseStatus = Lens.lens (responseStatus :: CreateCloudFormationStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCloudFormationStackResponse)
{-# DEPRECATED ccfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
