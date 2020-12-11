{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployment groups.
module Network.AWS.CodeDeploy.BatchGetDeploymentGroups
  ( -- * Creating a request
    BatchGetDeploymentGroups (..),
    mkBatchGetDeploymentGroups,

    -- ** Request lenses
    bgdgApplicationName,
    bgdgDeploymentGroupNames,

    -- * Destructuring the response
    BatchGetDeploymentGroupsResponse (..),
    mkBatchGetDeploymentGroupsResponse,

    -- ** Response lenses
    bgdgrsDeploymentGroupsInfo,
    bgdgrsErrorMessage,
    bgdgrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'mkBatchGetDeploymentGroups' smart constructor.
data BatchGetDeploymentGroups = BatchGetDeploymentGroups'
  { applicationName ::
      Lude.Text,
    deploymentGroupNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetDeploymentGroups' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
-- * 'deploymentGroupNames' - The names of the deployment groups.
mkBatchGetDeploymentGroups ::
  -- | 'applicationName'
  Lude.Text ->
  BatchGetDeploymentGroups
mkBatchGetDeploymentGroups pApplicationName_ =
  BatchGetDeploymentGroups'
    { applicationName = pApplicationName_,
      deploymentGroupNames = Lude.mempty
    }

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgApplicationName :: Lens.Lens' BatchGetDeploymentGroups Lude.Text
bgdgApplicationName = Lens.lens (applicationName :: BatchGetDeploymentGroups -> Lude.Text) (\s a -> s {applicationName = a} :: BatchGetDeploymentGroups)
{-# DEPRECATED bgdgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The names of the deployment groups.
--
-- /Note:/ Consider using 'deploymentGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgDeploymentGroupNames :: Lens.Lens' BatchGetDeploymentGroups [Lude.Text]
bgdgDeploymentGroupNames = Lens.lens (deploymentGroupNames :: BatchGetDeploymentGroups -> [Lude.Text]) (\s a -> s {deploymentGroupNames = a} :: BatchGetDeploymentGroups)
{-# DEPRECATED bgdgDeploymentGroupNames "Use generic-lens or generic-optics with 'deploymentGroupNames' instead." #-}

instance Lude.AWSRequest BatchGetDeploymentGroups where
  type Rs BatchGetDeploymentGroups = BatchGetDeploymentGroupsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetDeploymentGroupsResponse'
            Lude.<$> (x Lude..?> "deploymentGroupsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "errorMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetDeploymentGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.BatchGetDeploymentGroups" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetDeploymentGroups where
  toJSON BatchGetDeploymentGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("deploymentGroupNames" Lude..= deploymentGroupNames)
          ]
      )

instance Lude.ToPath BatchGetDeploymentGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetDeploymentGroups where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetDeploymentGroups@ operation.
--
-- /See:/ 'mkBatchGetDeploymentGroupsResponse' smart constructor.
data BatchGetDeploymentGroupsResponse = BatchGetDeploymentGroupsResponse'
  { deploymentGroupsInfo ::
      Lude.Maybe
        [DeploymentGroupInfo],
    errorMessage ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'BatchGetDeploymentGroupsResponse' with the minimum fields required to make a request.
--
-- * 'deploymentGroupsInfo' - Information about the deployment groups.
-- * 'errorMessage' - Information about errors that might have occurred during the API call.
-- * 'responseStatus' - The response status code.
mkBatchGetDeploymentGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetDeploymentGroupsResponse
mkBatchGetDeploymentGroupsResponse pResponseStatus_ =
  BatchGetDeploymentGroupsResponse'
    { deploymentGroupsInfo =
        Lude.Nothing,
      errorMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deployment groups.
--
-- /Note:/ Consider using 'deploymentGroupsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrsDeploymentGroupsInfo :: Lens.Lens' BatchGetDeploymentGroupsResponse (Lude.Maybe [DeploymentGroupInfo])
bgdgrsDeploymentGroupsInfo = Lens.lens (deploymentGroupsInfo :: BatchGetDeploymentGroupsResponse -> Lude.Maybe [DeploymentGroupInfo]) (\s a -> s {deploymentGroupsInfo = a} :: BatchGetDeploymentGroupsResponse)
{-# DEPRECATED bgdgrsDeploymentGroupsInfo "Use generic-lens or generic-optics with 'deploymentGroupsInfo' instead." #-}

-- | Information about errors that might have occurred during the API call.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrsErrorMessage :: Lens.Lens' BatchGetDeploymentGroupsResponse (Lude.Maybe Lude.Text)
bgdgrsErrorMessage = Lens.lens (errorMessage :: BatchGetDeploymentGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchGetDeploymentGroupsResponse)
{-# DEPRECATED bgdgrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdgrsResponseStatus :: Lens.Lens' BatchGetDeploymentGroupsResponse Lude.Int
bgdgrsResponseStatus = Lens.lens (responseStatus :: BatchGetDeploymentGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetDeploymentGroupsResponse)
{-# DEPRECATED bgdgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
