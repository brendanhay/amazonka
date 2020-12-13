{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resource descriptions for running and deleted stacks. If @StackName@ is specified, all the associated resources that are part of the stack are returned. If @PhysicalResourceId@ is specified, the associated resources of the stack that the resource belongs to are returned.
--
-- For deleted stacks, @DescribeStackResources@ returns resource information for up to 90 days after the stack has been deleted.
-- You must specify either @StackName@ or @PhysicalResourceId@ , but not both. In addition, you can specify @LogicalResourceId@ to filter the returned result. For more information about resources, the @LogicalResourceId@ and @PhysicalResourceId@ , go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/ AWS CloudFormation User Guide> .
module Network.AWS.CloudFormation.DescribeStackResources
  ( -- * Creating a request
    DescribeStackResources (..),
    mkDescribeStackResources,

    -- ** Request lenses
    dsrLogicalResourceId,
    dsrPhysicalResourceId,
    dsrStackName,

    -- * Destructuring the response
    DescribeStackResourcesResponse (..),
    mkDescribeStackResourcesResponse,

    -- ** Response lenses
    dsrrsStackResources,
    dsrrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'DescribeStackResources' action.
--
-- /See:/ 'mkDescribeStackResources' smart constructor.
data DescribeStackResources = DescribeStackResources'
  { -- | The logical name of the resource as specified in the template.
    --
    -- Default: There is no default value.
    logicalResourceId :: Lude.Maybe Lude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
    --
    -- For example, for an Amazon Elastic Compute Cloud (EC2) instance, @PhysicalResourceId@ corresponds to the @InstanceId@ . You can pass the EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the instance belongs to and what other resources are part of the stack.
    -- Required: Conditional. If you do not specify @PhysicalResourceId@ , you must specify @StackName@ .
    -- Default: There is no default value.
    physicalResourceId :: Lude.Maybe Lude.Text,
    -- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
    --
    --
    --     * Running stacks: You can specify either the stack's name or its unique stack ID.
    --
    --
    --     * Deleted stacks: You must specify the unique stack ID.
    --
    --
    -- Default: There is no default value.
    -- Required: Conditional. If you do not specify @StackName@ , you must specify @PhysicalResourceId@ .
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResources' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
-- * 'physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance, @PhysicalResourceId@ corresponds to the @InstanceId@ . You can pass the EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the instance belongs to and what other resources are part of the stack.
-- Required: Conditional. If you do not specify @PhysicalResourceId@ , you must specify @StackName@ .
-- Default: There is no default value.
-- * 'stackName' - The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
-- Required: Conditional. If you do not specify @StackName@ , you must specify @PhysicalResourceId@ .
mkDescribeStackResources ::
  DescribeStackResources
mkDescribeStackResources =
  DescribeStackResources'
    { logicalResourceId = Lude.Nothing,
      physicalResourceId = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrLogicalResourceId :: Lens.Lens' DescribeStackResources (Lude.Maybe Lude.Text)
dsrLogicalResourceId = Lens.lens (logicalResourceId :: DescribeStackResources -> Lude.Maybe Lude.Text) (\s a -> s {logicalResourceId = a} :: DescribeStackResources)
{-# DEPRECATED dsrLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The name or unique identifier that corresponds to a physical instance ID of a resource supported by AWS CloudFormation.
--
-- For example, for an Amazon Elastic Compute Cloud (EC2) instance, @PhysicalResourceId@ corresponds to the @InstanceId@ . You can pass the EC2 @InstanceId@ to @DescribeStackResources@ to find which stack the instance belongs to and what other resources are part of the stack.
-- Required: Conditional. If you do not specify @PhysicalResourceId@ , you must specify @StackName@ .
-- Default: There is no default value.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrPhysicalResourceId :: Lens.Lens' DescribeStackResources (Lude.Maybe Lude.Text)
dsrPhysicalResourceId = Lens.lens (physicalResourceId :: DescribeStackResources -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: DescribeStackResources)
{-# DEPRECATED dsrPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | The name or the unique stack ID that is associated with the stack, which are not always interchangeable:
--
--
--     * Running stacks: You can specify either the stack's name or its unique stack ID.
--
--
--     * Deleted stacks: You must specify the unique stack ID.
--
--
-- Default: There is no default value.
-- Required: Conditional. If you do not specify @StackName@ , you must specify @PhysicalResourceId@ .
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrStackName :: Lens.Lens' DescribeStackResources (Lude.Maybe Lude.Text)
dsrStackName = Lens.lens (stackName :: DescribeStackResources -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: DescribeStackResources)
{-# DEPRECATED dsrStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DescribeStackResources where
  type Rs DescribeStackResources = DescribeStackResourcesResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackResourcesResult"
      ( \s h x ->
          DescribeStackResourcesResponse'
            Lude.<$> ( x Lude..@? "StackResources" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackResources where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackResources where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackResources where
  toQuery DescribeStackResources' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStackResources" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "LogicalResourceId" Lude.=: logicalResourceId,
        "PhysicalResourceId" Lude.=: physicalResourceId,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'DescribeStackResources' action.
--
-- /See:/ 'mkDescribeStackResourcesResponse' smart constructor.
data DescribeStackResourcesResponse = DescribeStackResourcesResponse'
  { -- | A list of @StackResource@ structures.
    stackResources :: Lude.Maybe [StackResource],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResourcesResponse' with the minimum fields required to make a request.
--
-- * 'stackResources' - A list of @StackResource@ structures.
-- * 'responseStatus' - The response status code.
mkDescribeStackResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackResourcesResponse
mkDescribeStackResourcesResponse pResponseStatus_ =
  DescribeStackResourcesResponse'
    { stackResources = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @StackResource@ structures.
--
-- /Note:/ Consider using 'stackResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsStackResources :: Lens.Lens' DescribeStackResourcesResponse (Lude.Maybe [StackResource])
dsrrsStackResources = Lens.lens (stackResources :: DescribeStackResourcesResponse -> Lude.Maybe [StackResource]) (\s a -> s {stackResources = a} :: DescribeStackResourcesResponse)
{-# DEPRECATED dsrrsStackResources "Use generic-lens or generic-optics with 'stackResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeStackResourcesResponse Lude.Int
dsrrsResponseStatus = Lens.lens (responseStatus :: DescribeStackResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackResourcesResponse)
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
