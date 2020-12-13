{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.RegisterDomain
  ( -- * Creating a request
    RegisterDomain (..),
    mkRegisterDomain,

    -- ** Request lenses
    rdName,
    rdWorkflowExecutionRetentionPeriodInDays,
    rdDescription,
    rdTags,

    -- * Destructuring the response
    RegisterDomainResponse (..),
    mkRegisterDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkRegisterDomain' smart constructor.
data RegisterDomain = RegisterDomain'
  { -- | Name of the domain to register. The name must be unique in the region that the domain is registered in.
    --
    -- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
    name :: Lude.Text,
    -- | The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls.
    --
    -- If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted.
    -- The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
    workflowExecutionRetentionPeriodInDays :: Lude.Text,
    -- | A text description of the domain.
    description :: Lude.Maybe Lude.Text,
    -- | Tags to be added when registering a domain.
    --
    -- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
    tags :: Lude.Maybe [ResourceTag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDomain' with the minimum fields required to make a request.
--
-- * 'name' - Name of the domain to register. The name must be unique in the region that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'workflowExecutionRetentionPeriodInDays' - The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted.
-- The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
-- * 'description' - A text description of the domain.
-- * 'tags' - Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
mkRegisterDomain ::
  -- | 'name'
  Lude.Text ->
  -- | 'workflowExecutionRetentionPeriodInDays'
  Lude.Text ->
  RegisterDomain
mkRegisterDomain pName_ pWorkflowExecutionRetentionPeriodInDays_ =
  RegisterDomain'
    { name = pName_,
      workflowExecutionRetentionPeriodInDays =
        pWorkflowExecutionRetentionPeriodInDays_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Name of the domain to register. The name must be unique in the region that the domain is registered in.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdName :: Lens.Lens' RegisterDomain Lude.Text
rdName = Lens.lens (name :: RegisterDomain -> Lude.Text) (\s a -> s {name = a} :: RegisterDomain)
{-# DEPRECATED rdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The duration (in days) that records and histories of workflow executions on the domain should be kept by the service. After the retention period, the workflow execution isn't available in the results of visibility calls.
--
-- If you pass the value @NONE@ or @0@ (zero), then the workflow execution history isn't retained. As soon as the workflow execution completes, the execution record and its history are deleted.
-- The maximum workflow execution retention period is 90 days. For more information about Amazon SWF service limits, see: <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-limits.html Amazon SWF Service Limits> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'workflowExecutionRetentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdWorkflowExecutionRetentionPeriodInDays :: Lens.Lens' RegisterDomain Lude.Text
rdWorkflowExecutionRetentionPeriodInDays = Lens.lens (workflowExecutionRetentionPeriodInDays :: RegisterDomain -> Lude.Text) (\s a -> s {workflowExecutionRetentionPeriodInDays = a} :: RegisterDomain)
{-# DEPRECATED rdWorkflowExecutionRetentionPeriodInDays "Use generic-lens or generic-optics with 'workflowExecutionRetentionPeriodInDays' instead." #-}

-- | A text description of the domain.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDescription :: Lens.Lens' RegisterDomain (Lude.Maybe Lude.Text)
rdDescription = Lens.lens (description :: RegisterDomain -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegisterDomain)
{-# DEPRECATED rdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Tags to be added when registering a domain.
--
-- Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdTags :: Lens.Lens' RegisterDomain (Lude.Maybe [ResourceTag])
rdTags = Lens.lens (tags :: RegisterDomain -> Lude.Maybe [ResourceTag]) (\s a -> s {tags = a} :: RegisterDomain)
{-# DEPRECATED rdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RegisterDomain where
  type Rs RegisterDomain = RegisterDomainResponse
  request = Req.postJSON swfService
  response = Res.receiveNull RegisterDomainResponse'

instance Lude.ToHeaders RegisterDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.RegisterDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterDomain where
  toJSON RegisterDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just
              ( "workflowExecutionRetentionPeriodInDays"
                  Lude..= workflowExecutionRetentionPeriodInDays
              ),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath RegisterDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDomainResponse' with the minimum fields required to make a request.
mkRegisterDomainResponse ::
  RegisterDomainResponse
mkRegisterDomainResponse = RegisterDomainResponse'
