{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified resource in the specified stack.
--
-- For deleted stacks, DescribeStackResource returns resource information for up to 90 days after the stack has been deleted.
module Network.AWS.CloudFormation.DescribeStackResource
  ( -- * Creating a request
    DescribeStackResource (..),
    mkDescribeStackResource,

    -- ** Request lenses
    dsrfLogicalResourceId,
    dsrfStackName,

    -- * Destructuring the response
    DescribeStackResourceResponse (..),
    mkDescribeStackResourceResponse,

    -- ** Response lenses
    dsrfrsStackResourceDetail,
    dsrfrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'DescribeStackResource' action.
--
-- /See:/ 'mkDescribeStackResource' smart constructor.
data DescribeStackResource = DescribeStackResource'
  { -- | The logical name of the resource as specified in the template.
    --
    -- Default: There is no default value.
    logicalResourceId :: Lude.Text,
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
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResource' with the minimum fields required to make a request.
--
-- * 'logicalResourceId' - The logical name of the resource as specified in the template.
--
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
mkDescribeStackResource ::
  -- | 'logicalResourceId'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  DescribeStackResource
mkDescribeStackResource pLogicalResourceId_ pStackName_ =
  DescribeStackResource'
    { logicalResourceId = pLogicalResourceId_,
      stackName = pStackName_
    }

-- | The logical name of the resource as specified in the template.
--
-- Default: There is no default value.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfLogicalResourceId :: Lens.Lens' DescribeStackResource Lude.Text
dsrfLogicalResourceId = Lens.lens (logicalResourceId :: DescribeStackResource -> Lude.Text) (\s a -> s {logicalResourceId = a} :: DescribeStackResource)
{-# DEPRECATED dsrfLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

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
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfStackName :: Lens.Lens' DescribeStackResource Lude.Text
dsrfStackName = Lens.lens (stackName :: DescribeStackResource -> Lude.Text) (\s a -> s {stackName = a} :: DescribeStackResource)
{-# DEPRECATED dsrfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest DescribeStackResource where
  type Rs DescribeStackResource = DescribeStackResourceResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeStackResourceResult"
      ( \s h x ->
          DescribeStackResourceResponse'
            Lude.<$> (x Lude..@? "StackResourceDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStackResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackResource where
  toQuery DescribeStackResource' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeStackResource" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "LogicalResourceId" Lude.=: logicalResourceId,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'DescribeStackResource' action.
--
-- /See:/ 'mkDescribeStackResourceResponse' smart constructor.
data DescribeStackResourceResponse = DescribeStackResourceResponse'
  { -- | A @StackResourceDetail@ structure containing the description of the specified resource in the specified stack.
    stackResourceDetail :: Lude.Maybe StackResourceDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStackResourceResponse' with the minimum fields required to make a request.
--
-- * 'stackResourceDetail' - A @StackResourceDetail@ structure containing the description of the specified resource in the specified stack.
-- * 'responseStatus' - The response status code.
mkDescribeStackResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackResourceResponse
mkDescribeStackResourceResponse pResponseStatus_ =
  DescribeStackResourceResponse'
    { stackResourceDetail =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @StackResourceDetail@ structure containing the description of the specified resource in the specified stack.
--
-- /Note:/ Consider using 'stackResourceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsStackResourceDetail :: Lens.Lens' DescribeStackResourceResponse (Lude.Maybe StackResourceDetail)
dsrfrsStackResourceDetail = Lens.lens (stackResourceDetail :: DescribeStackResourceResponse -> Lude.Maybe StackResourceDetail) (\s a -> s {stackResourceDetail = a} :: DescribeStackResourceResponse)
{-# DEPRECATED dsrfrsStackResourceDetail "Use generic-lens or generic-optics with 'stackResourceDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeStackResourceResponse Lude.Int
dsrfrsResponseStatus = Lens.lens (responseStatus :: DescribeStackResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackResourceResponse)
{-# DEPRECATED dsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
