{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DeprecateWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /workflow type/ . After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.
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
module Network.AWS.SWF.DeprecateWorkflowType
  ( -- * Creating a request
    DeprecateWorkflowType (..),
    mkDeprecateWorkflowType,

    -- ** Request lenses
    dwtDomain,
    dwtWorkflowType,

    -- * Destructuring the response
    DeprecateWorkflowTypeResponse (..),
    mkDeprecateWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDeprecateWorkflowType' smart constructor.
data DeprecateWorkflowType = DeprecateWorkflowType'
  { -- | The name of the domain in which the workflow type is registered.
    domain :: Lude.Text,
    -- | The workflow type to deprecate.
    workflowType :: WorkflowType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateWorkflowType' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain in which the workflow type is registered.
-- * 'workflowType' - The workflow type to deprecate.
mkDeprecateWorkflowType ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  DeprecateWorkflowType
mkDeprecateWorkflowType pDomain_ pWorkflowType_ =
  DeprecateWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain in which the workflow type is registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtDomain :: Lens.Lens' DeprecateWorkflowType Lude.Text
dwtDomain = Lens.lens (domain :: DeprecateWorkflowType -> Lude.Text) (\s a -> s {domain = a} :: DeprecateWorkflowType)
{-# DEPRECATED dwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflow type to deprecate.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwtWorkflowType :: Lens.Lens' DeprecateWorkflowType WorkflowType
dwtWorkflowType = Lens.lens (workflowType :: DeprecateWorkflowType -> WorkflowType) (\s a -> s {workflowType = a} :: DeprecateWorkflowType)
{-# DEPRECATED dwtWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Lude.AWSRequest DeprecateWorkflowType where
  type Rs DeprecateWorkflowType = DeprecateWorkflowTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull DeprecateWorkflowTypeResponse'

instance Lude.ToHeaders DeprecateWorkflowType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.DeprecateWorkflowType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeprecateWorkflowType where
  toJSON DeprecateWorkflowType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("workflowType" Lude..= workflowType)
          ]
      )

instance Lude.ToPath DeprecateWorkflowType where
  toPath = Lude.const "/"

instance Lude.ToQuery DeprecateWorkflowType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeprecateWorkflowTypeResponse' smart constructor.
data DeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeprecateWorkflowTypeResponse' with the minimum fields required to make a request.
mkDeprecateWorkflowTypeResponse ::
  DeprecateWorkflowTypeResponse
mkDeprecateWorkflowTypeResponse = DeprecateWorkflowTypeResponse'
