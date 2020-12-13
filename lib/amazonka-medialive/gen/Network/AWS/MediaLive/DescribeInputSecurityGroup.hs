{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a summary of an Input Security Group
module Network.AWS.MediaLive.DescribeInputSecurityGroup
  ( -- * Creating a request
    DescribeInputSecurityGroup (..),
    mkDescribeInputSecurityGroup,

    -- ** Request lenses
    dInputSecurityGroupId,

    -- * Destructuring the response
    DescribeInputSecurityGroupResponse (..),
    mkDescribeInputSecurityGroupResponse,

    -- ** Response lenses
    disgfrsState,
    disgfrsARN,
    disgfrsInputs,
    disgfrsId,
    disgfrsWhitelistRules,
    disgfrsTags,
    disgfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeInputSecurityGroupRequest
--
-- /See:/ 'mkDescribeInputSecurityGroup' smart constructor.
newtype DescribeInputSecurityGroup = DescribeInputSecurityGroup'
  { -- | The id of the Input Security Group to describe
    inputSecurityGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputSecurityGroup' with the minimum fields required to make a request.
--
-- * 'inputSecurityGroupId' - The id of the Input Security Group to describe
mkDescribeInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Lude.Text ->
  DescribeInputSecurityGroup
mkDescribeInputSecurityGroup pInputSecurityGroupId_ =
  DescribeInputSecurityGroup'
    { inputSecurityGroupId =
        pInputSecurityGroupId_
    }

-- | The id of the Input Security Group to describe
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInputSecurityGroupId :: Lens.Lens' DescribeInputSecurityGroup Lude.Text
dInputSecurityGroupId = Lens.lens (inputSecurityGroupId :: DescribeInputSecurityGroup -> Lude.Text) (\s a -> s {inputSecurityGroupId = a} :: DescribeInputSecurityGroup)
{-# DEPRECATED dInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

instance Lude.AWSRequest DescribeInputSecurityGroup where
  type
    Rs DescribeInputSecurityGroup =
      DescribeInputSecurityGroupResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInputSecurityGroupResponse'
            Lude.<$> (x Lude..?> "state")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "inputs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "whitelistRules" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInputSecurityGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInputSecurityGroup where
  toPath DescribeInputSecurityGroup' {..} =
    Lude.mconcat
      ["/prod/inputSecurityGroups/", Lude.toBS inputSecurityGroupId]

instance Lude.ToQuery DescribeInputSecurityGroup where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeInputSecurityGroupResponse
--
-- /See:/ 'mkDescribeInputSecurityGroupResponse' smart constructor.
data DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse'
  { -- | The current state of the Input Security Group.
    state :: Lude.Maybe InputSecurityGroupState,
    -- | Unique ARN of Input Security Group
    arn :: Lude.Maybe Lude.Text,
    -- | The list of inputs currently using this Input Security Group.
    inputs :: Lude.Maybe [Lude.Text],
    -- | The Id of the Input Security Group
    id :: Lude.Maybe Lude.Text,
    -- | Whitelist rules and their sync status
    whitelistRules :: Lude.Maybe [InputWhitelistRule],
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the Input Security Group.
-- * 'arn' - Unique ARN of Input Security Group
-- * 'inputs' - The list of inputs currently using this Input Security Group.
-- * 'id' - The Id of the Input Security Group
-- * 'whitelistRules' - Whitelist rules and their sync status
-- * 'tags' - A collection of key-value pairs.
-- * 'responseStatus' - The response status code.
mkDescribeInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInputSecurityGroupResponse
mkDescribeInputSecurityGroupResponse pResponseStatus_ =
  DescribeInputSecurityGroupResponse'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      inputs = Lude.Nothing,
      id = Lude.Nothing,
      whitelistRules = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the Input Security Group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsState :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe InputSecurityGroupState)
disgfrsState = Lens.lens (state :: DescribeInputSecurityGroupResponse -> Lude.Maybe InputSecurityGroupState) (\s a -> s {state = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsARN :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe Lude.Text)
disgfrsARN = Lens.lens (arn :: DescribeInputSecurityGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsInputs :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe [Lude.Text])
disgfrsInputs = Lens.lens (inputs :: DescribeInputSecurityGroupResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {inputs = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsId :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe Lude.Text)
disgfrsId = Lens.lens (id :: DescribeInputSecurityGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsWhitelistRules :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe [InputWhitelistRule])
disgfrsWhitelistRules = Lens.lens (whitelistRules :: DescribeInputSecurityGroupResponse -> Lude.Maybe [InputWhitelistRule]) (\s a -> s {whitelistRules = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsTags :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
disgfrsTags = Lens.lens (tags :: DescribeInputSecurityGroupResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgfrsResponseStatus :: Lens.Lens' DescribeInputSecurityGroupResponse Lude.Int
disgfrsResponseStatus = Lens.lens (responseStatus :: DescribeInputSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED disgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
