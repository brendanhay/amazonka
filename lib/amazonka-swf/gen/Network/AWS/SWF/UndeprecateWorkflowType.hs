{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.UndeprecateWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undeprecates a previously deprecated /workflow type/ . After a workflow type has been undeprecated, you can create new executions of that type.
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
module Network.AWS.SWF.UndeprecateWorkflowType
  ( -- * Creating a request
    UndeprecateWorkflowType (..),
    mkUndeprecateWorkflowType,

    -- ** Request lenses
    uwtDomain,
    uwtWorkflowType,

    -- * Destructuring the response
    UndeprecateWorkflowTypeResponse (..),
    mkUndeprecateWorkflowTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkUndeprecateWorkflowType' smart constructor.
data UndeprecateWorkflowType = UndeprecateWorkflowType'
  { domain ::
      Lude.Text,
    workflowType :: WorkflowType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateWorkflowType' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain of the deprecated workflow type.
-- * 'workflowType' - The name of the domain of the deprecated workflow type.
mkUndeprecateWorkflowType ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  UndeprecateWorkflowType
mkUndeprecateWorkflowType pDomain_ pWorkflowType_ =
  UndeprecateWorkflowType'
    { domain = pDomain_,
      workflowType = pWorkflowType_
    }

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwtDomain :: Lens.Lens' UndeprecateWorkflowType Lude.Text
uwtDomain = Lens.lens (domain :: UndeprecateWorkflowType -> Lude.Text) (\s a -> s {domain = a} :: UndeprecateWorkflowType)
{-# DEPRECATED uwtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the domain of the deprecated workflow type.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwtWorkflowType :: Lens.Lens' UndeprecateWorkflowType WorkflowType
uwtWorkflowType = Lens.lens (workflowType :: UndeprecateWorkflowType -> WorkflowType) (\s a -> s {workflowType = a} :: UndeprecateWorkflowType)
{-# DEPRECATED uwtWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

instance Lude.AWSRequest UndeprecateWorkflowType where
  type Rs UndeprecateWorkflowType = UndeprecateWorkflowTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull UndeprecateWorkflowTypeResponse'

instance Lude.ToHeaders UndeprecateWorkflowType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.UndeprecateWorkflowType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UndeprecateWorkflowType where
  toJSON UndeprecateWorkflowType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("workflowType" Lude..= workflowType)
          ]
      )

instance Lude.ToPath UndeprecateWorkflowType where
  toPath = Lude.const "/"

instance Lude.ToQuery UndeprecateWorkflowType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUndeprecateWorkflowTypeResponse' smart constructor.
data UndeprecateWorkflowTypeResponse = UndeprecateWorkflowTypeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UndeprecateWorkflowTypeResponse' with the minimum fields required to make a request.
mkUndeprecateWorkflowTypeResponse ::
  UndeprecateWorkflowTypeResponse
mkUndeprecateWorkflowTypeResponse =
  UndeprecateWorkflowTypeResponse'
