{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a stack's provisioning parameters.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
  ( -- * Creating a request
    DescribeStackProvisioningParameters (..),
    mkDescribeStackProvisioningParameters,

    -- ** Request lenses
    dsppStackId,

    -- * Destructuring the response
    DescribeStackProvisioningParametersResponse (..),
    mkDescribeStackProvisioningParametersResponse,

    -- ** Response lenses
    dspprsAgentInstallerURL,
    dspprsParameters,
    dspprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStackProvisioningParameters' smart constructor.
newtype DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
  { stackId ::
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

-- | Creates a value of 'DescribeStackProvisioningParameters' with the minimum fields required to make a request.
--
-- * 'stackId' - The stack ID.
mkDescribeStackProvisioningParameters ::
  -- | 'stackId'
  Lude.Text ->
  DescribeStackProvisioningParameters
mkDescribeStackProvisioningParameters pStackId_ =
  DescribeStackProvisioningParameters' {stackId = pStackId_}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsppStackId :: Lens.Lens' DescribeStackProvisioningParameters Lude.Text
dsppStackId = Lens.lens (stackId :: DescribeStackProvisioningParameters -> Lude.Text) (\s a -> s {stackId = a} :: DescribeStackProvisioningParameters)
{-# DEPRECATED dsppStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeStackProvisioningParameters where
  type
    Rs DescribeStackProvisioningParameters =
      DescribeStackProvisioningParametersResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStackProvisioningParametersResponse'
            Lude.<$> (x Lude..?> "AgentInstallerUrl")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStackProvisioningParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorks_20130218.DescribeStackProvisioningParameters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStackProvisioningParameters where
  toJSON DescribeStackProvisioningParameters' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StackId" Lude..= stackId)])

instance Lude.ToPath DescribeStackProvisioningParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStackProvisioningParameters where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeStackProvisioningParameters@ request.
--
-- /See:/ 'mkDescribeStackProvisioningParametersResponse' smart constructor.
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
  { agentInstallerURL ::
      Lude.Maybe
        Lude.Text,
    parameters ::
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

-- | Creates a value of 'DescribeStackProvisioningParametersResponse' with the minimum fields required to make a request.
--
-- * 'agentInstallerURL' - The AWS OpsWorks Stacks agent installer's URL.
-- * 'parameters' - An embedded object that contains the provisioning parameters.
-- * 'responseStatus' - The response status code.
mkDescribeStackProvisioningParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStackProvisioningParametersResponse
mkDescribeStackProvisioningParametersResponse pResponseStatus_ =
  DescribeStackProvisioningParametersResponse'
    { agentInstallerURL =
        Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS OpsWorks Stacks agent installer's URL.
--
-- /Note:/ Consider using 'agentInstallerURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprsAgentInstallerURL :: Lens.Lens' DescribeStackProvisioningParametersResponse (Lude.Maybe Lude.Text)
dspprsAgentInstallerURL = Lens.lens (agentInstallerURL :: DescribeStackProvisioningParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {agentInstallerURL = a} :: DescribeStackProvisioningParametersResponse)
{-# DEPRECATED dspprsAgentInstallerURL "Use generic-lens or generic-optics with 'agentInstallerURL' instead." #-}

-- | An embedded object that contains the provisioning parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprsParameters :: Lens.Lens' DescribeStackProvisioningParametersResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dspprsParameters = Lens.lens (parameters :: DescribeStackProvisioningParametersResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: DescribeStackProvisioningParametersResponse)
{-# DEPRECATED dspprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprsResponseStatus :: Lens.Lens' DescribeStackProvisioningParametersResponse Lude.Int
dspprsResponseStatus = Lens.lens (responseStatus :: DescribeStackProvisioningParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStackProvisioningParametersResponse)
{-# DEPRECATED dspprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
