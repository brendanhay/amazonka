{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeAgentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS OpsWorks Stacks agent versions. You must specify a stack ID or a configuration manager. @DescribeAgentVersions@ returns a list of available agent versions for the specified stack or configuration manager.
module Network.AWS.OpsWorks.DescribeAgentVersions
  ( -- * Creating a request
    DescribeAgentVersions (..),
    mkDescribeAgentVersions,

    -- ** Request lenses
    davConfigurationManager,
    davStackId,

    -- * Destructuring the response
    DescribeAgentVersionsResponse (..),
    mkDescribeAgentVersionsResponse,

    -- ** Response lenses
    davrsAgentVersions,
    davrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAgentVersions' smart constructor.
data DescribeAgentVersions = DescribeAgentVersions'
  { configurationManager ::
      Lude.Maybe StackConfigurationManager,
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAgentVersions' with the minimum fields required to make a request.
--
-- * 'configurationManager' - The configuration manager.
-- * 'stackId' - The stack ID.
mkDescribeAgentVersions ::
  DescribeAgentVersions
mkDescribeAgentVersions =
  DescribeAgentVersions'
    { configurationManager = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The configuration manager.
--
-- /Note:/ Consider using 'configurationManager' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davConfigurationManager :: Lens.Lens' DescribeAgentVersions (Lude.Maybe StackConfigurationManager)
davConfigurationManager = Lens.lens (configurationManager :: DescribeAgentVersions -> Lude.Maybe StackConfigurationManager) (\s a -> s {configurationManager = a} :: DescribeAgentVersions)
{-# DEPRECATED davConfigurationManager "Use generic-lens or generic-optics with 'configurationManager' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davStackId :: Lens.Lens' DescribeAgentVersions (Lude.Maybe Lude.Text)
davStackId = Lens.lens (stackId :: DescribeAgentVersions -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeAgentVersions)
{-# DEPRECATED davStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeAgentVersions where
  type Rs DescribeAgentVersions = DescribeAgentVersionsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAgentVersionsResponse'
            Lude.<$> (x Lude..?> "AgentVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAgentVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeAgentVersions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAgentVersions where
  toJSON DescribeAgentVersions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConfigurationManager" Lude..=) Lude.<$> configurationManager,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeAgentVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAgentVersions where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeAgentVersions@ request.
--
-- /See:/ 'mkDescribeAgentVersionsResponse' smart constructor.
data DescribeAgentVersionsResponse = DescribeAgentVersionsResponse'
  { agentVersions ::
      Lude.Maybe [AgentVersion],
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

-- | Creates a value of 'DescribeAgentVersionsResponse' with the minimum fields required to make a request.
--
-- * 'agentVersions' - The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
-- * 'responseStatus' - The response status code.
mkDescribeAgentVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAgentVersionsResponse
mkDescribeAgentVersionsResponse pResponseStatus_ =
  DescribeAgentVersionsResponse'
    { agentVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The agent versions for the specified stack or configuration manager. Note that this value is the complete version number, not the abbreviated number used by the console.
--
-- /Note:/ Consider using 'agentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrsAgentVersions :: Lens.Lens' DescribeAgentVersionsResponse (Lude.Maybe [AgentVersion])
davrsAgentVersions = Lens.lens (agentVersions :: DescribeAgentVersionsResponse -> Lude.Maybe [AgentVersion]) (\s a -> s {agentVersions = a} :: DescribeAgentVersionsResponse)
{-# DEPRECATED davrsAgentVersions "Use generic-lens or generic-optics with 'agentVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davrsResponseStatus :: Lens.Lens' DescribeAgentVersionsResponse Lude.Int
davrsResponseStatus = Lens.lens (responseStatus :: DescribeAgentVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAgentVersionsResponse)
{-# DEPRECATED davrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
