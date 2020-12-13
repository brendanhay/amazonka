{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified /workflow type/ . This includes configuration settings specified when the type was registered and other information such as creation date, current status, etc.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @workflowType.name@ : String constraint. The key is @swf:workflowType.name@ .
--
--
--     * @workflowType.version@ : String constraint. The key is @swf:workflowType.version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DescribeWorkflowType
  ( -- * Creating a request
    DescribeWorkflowType (..),
    mkDescribeWorkflowType,

    -- ** Request lenses
    dDomain,
    dWorkflowType,

    -- * Destructuring the response
    DescribeWorkflowTypeResponse (..),
    mkDescribeWorkflowTypeResponse,

    -- ** Response lenses
    dwtrsTypeInfo,
    dwtrsConfiguration,
    dwtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDescribeWorkflowType' smart constructor.
data DescribeWorkflowType = DescribeWorkflowType'
  { -- | The name of the domain in which this workflow type is registered.
    domain :: Lude.Text,
    -- | The workflow type to describe.
    workflowType :: WorkflowType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkflowType' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain in which this workflow type is registered.
-- * 'workflowType' - The workflow type to describe.
mkDescribeWorkflowType ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  DescribeWorkflowType
mkDescribeWorkflowType pDomain_ pWorkflowType_ =
  DescribeWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain in which this workflow type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomain :: Lens.Lens' DescribeWorkflowType Lude.Text
dDomain = Lens.lens (domain :: DescribeWorkflowType -> Lude.Text) (\s a -> s {domain = a} :: DescribeWorkflowType)
{-# DEPRECATED dDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflow type to describe.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkflowType :: Lens.Lens' DescribeWorkflowType WorkflowType
dWorkflowType = Lens.lens (workflowType :: DescribeWorkflowType -> WorkflowType) (\s a -> s {workflowType = a} :: DescribeWorkflowType)
{-# DEPRECATED dWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Lude.AWSRequest DescribeWorkflowType where
  type Rs DescribeWorkflowType = DescribeWorkflowTypeResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeWorkflowTypeResponse'
            Lude.<$> (x Lude..:> "typeInfo")
            Lude.<*> (x Lude..:> "configuration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeWorkflowType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.DescribeWorkflowType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeWorkflowType where
  toJSON DescribeWorkflowType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("workflowType" Lude..= workflowType)
          ]
      )

instance Lude.ToPath DescribeWorkflowType where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeWorkflowType where
  toQuery = Lude.const Lude.mempty

-- | Contains details about a workflow type.
--
-- /See:/ 'mkDescribeWorkflowTypeResponse' smart constructor.
data DescribeWorkflowTypeResponse = DescribeWorkflowTypeResponse'
  { -- | General information about the workflow type.
    --
    -- The status of the workflow type (returned in the WorkflowTypeInfo structure) can be one of the following.
    --
    --     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running.
    --
    --
    --     * @DEPRECATED@ – The type was deprecated using 'DeprecateWorkflowType' , but is still in use. You should keep workers supporting this type running. You cannot create new workflow executions of this type.
    typeInfo :: WorkflowTypeInfo,
    -- | Configuration settings of the workflow type registered through 'RegisterWorkflowType'
    configuration :: WorkflowTypeConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeWorkflowTypeResponse' with the minimum fields required to make a request.
--
-- * 'typeInfo' - General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo structure) can be one of the following.
--
--     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running.
--
--
--     * @DEPRECATED@ – The type was deprecated using 'DeprecateWorkflowType' , but is still in use. You should keep workers supporting this type running. You cannot create new workflow executions of this type.
--
--
-- * 'configuration' - Configuration settings of the workflow type registered through 'RegisterWorkflowType'
-- * 'responseStatus' - The response status code.
mkDescribeWorkflowTypeResponse ::
  -- | 'typeInfo'
  WorkflowTypeInfo ->
  -- | 'configuration'
  WorkflowTypeConfiguration ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeWorkflowTypeResponse
mkDescribeWorkflowTypeResponse
  pTypeInfo_
  pConfiguration_
  pResponseStatus_ =
    DescribeWorkflowTypeResponse'
      { typeInfo = pTypeInfo_,
        configuration = pConfiguration_,
        responseStatus = pResponseStatus_
      }

-- | General information about the workflow type.
--
-- The status of the workflow type (returned in the WorkflowTypeInfo structure) can be one of the following.
--
--     * @REGISTERED@ – The type is registered and available. Workers supporting this type should be running.
--
--
--     * @DEPRECATED@ – The type was deprecated using 'DeprecateWorkflowType' , but is still in use. You should keep workers supporting this type running. You cannot create new workflow executions of this type.
--
--
--
-- /Note:/ Consider using 'typeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrsTypeInfo :: Lens.Lens' DescribeWorkflowTypeResponse WorkflowTypeInfo
dwtrsTypeInfo = Lens.lens (typeInfo :: DescribeWorkflowTypeResponse -> WorkflowTypeInfo) (\s a -> s {typeInfo = a} :: DescribeWorkflowTypeResponse)
{-# DEPRECATED dwtrsTypeInfo "Use generic-lens or generic-optics with 'typeInfo' instead." #-}

-- | Configuration settings of the workflow type registered through 'RegisterWorkflowType'
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrsConfiguration :: Lens.Lens' DescribeWorkflowTypeResponse WorkflowTypeConfiguration
dwtrsConfiguration = Lens.lens (configuration :: DescribeWorkflowTypeResponse -> WorkflowTypeConfiguration) (\s a -> s {configuration = a} :: DescribeWorkflowTypeResponse)
{-# DEPRECATED dwtrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtrsResponseStatus :: Lens.Lens' DescribeWorkflowTypeResponse Lude.Int
dwtrsResponseStatus = Lens.lens (responseStatus :: DescribeWorkflowTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeWorkflowTypeResponse)
{-# DEPRECATED dwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
