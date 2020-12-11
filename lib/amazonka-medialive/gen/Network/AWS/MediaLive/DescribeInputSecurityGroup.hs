{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    disgInputSecurityGroupId,

    -- * Destructuring the response
    DescribeInputSecurityGroupResponse (..),
    mkDescribeInputSecurityGroupResponse,

    -- ** Response lenses
    desrsState,
    desrsARN,
    desrsInputs,
    desrsId,
    desrsWhitelistRules,
    desrsTags,
    desrsResponseStatus,
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
  { inputSecurityGroupId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
disgInputSecurityGroupId :: Lens.Lens' DescribeInputSecurityGroup Lude.Text
disgInputSecurityGroupId = Lens.lens (inputSecurityGroupId :: DescribeInputSecurityGroup -> Lude.Text) (\s a -> s {inputSecurityGroupId = a} :: DescribeInputSecurityGroup)
{-# DEPRECATED disgInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

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
  { state ::
      Lude.Maybe
        InputSecurityGroupState,
    arn ::
      Lude.Maybe Lude.Text,
    inputs ::
      Lude.Maybe
        [Lude.Text],
    id ::
      Lude.Maybe Lude.Text,
    whitelistRules ::
      Lude.Maybe
        [InputWhitelistRule],
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- * 'arn' - Unique ARN of Input Security Group
-- * 'id' - The Id of the Input Security Group
-- * 'inputs' - The list of inputs currently using this Input Security Group.
-- * 'responseStatus' - The response status code.
-- * 'state' - The current state of the Input Security Group.
-- * 'tags' - A collection of key-value pairs.
-- * 'whitelistRules' - Whitelist rules and their sync status
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
desrsState :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe InputSecurityGroupState)
desrsState = Lens.lens (state :: DescribeInputSecurityGroupResponse -> Lude.Maybe InputSecurityGroupState) (\s a -> s {state = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsARN :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe Lude.Text)
desrsARN = Lens.lens (arn :: DescribeInputSecurityGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsInputs :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe [Lude.Text])
desrsInputs = Lens.lens (inputs :: DescribeInputSecurityGroupResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {inputs = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsId :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe Lude.Text)
desrsId = Lens.lens (id :: DescribeInputSecurityGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsWhitelistRules :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe [InputWhitelistRule])
desrsWhitelistRules = Lens.lens (whitelistRules :: DescribeInputSecurityGroupResponse -> Lude.Maybe [InputWhitelistRule]) (\s a -> s {whitelistRules = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsTags :: Lens.Lens' DescribeInputSecurityGroupResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
desrsTags = Lens.lens (tags :: DescribeInputSecurityGroupResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeInputSecurityGroupResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeInputSecurityGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputSecurityGroupResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
