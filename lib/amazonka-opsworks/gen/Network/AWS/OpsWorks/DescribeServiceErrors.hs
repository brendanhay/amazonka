{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes AWS OpsWorks Stacks service errors.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- This call accepts only one resource-identifying parameter.
module Network.AWS.OpsWorks.DescribeServiceErrors
  ( -- * Creating a request
    DescribeServiceErrors (..),
    mkDescribeServiceErrors,

    -- ** Request lenses
    dseInstanceId,
    dseStackId,
    dseServiceErrorIds,

    -- * Destructuring the response
    DescribeServiceErrorsResponse (..),
    mkDescribeServiceErrorsResponse,

    -- ** Response lenses
    dsersServiceErrors,
    dsersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeServiceErrors' smart constructor.
data DescribeServiceErrors = DescribeServiceErrors'
  { -- | The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
    serviceErrorIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceErrors' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
-- * 'stackId' - The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
-- * 'serviceErrorIds' - An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
mkDescribeServiceErrors ::
  DescribeServiceErrors
mkDescribeServiceErrors =
  DescribeServiceErrors'
    { instanceId = Lude.Nothing,
      stackId = Lude.Nothing,
      serviceErrorIds = Lude.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseInstanceId :: Lens.Lens' DescribeServiceErrors (Lude.Maybe Lude.Text)
dseInstanceId = Lens.lens (instanceId :: DescribeServiceErrors -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeServiceErrors)
{-# DEPRECATED dseInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseStackId :: Lens.Lens' DescribeServiceErrors (Lude.Maybe Lude.Text)
dseStackId = Lens.lens (stackId :: DescribeServiceErrors -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeServiceErrors)
{-# DEPRECATED dseStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
--
-- /Note:/ Consider using 'serviceErrorIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseServiceErrorIds :: Lens.Lens' DescribeServiceErrors (Lude.Maybe [Lude.Text])
dseServiceErrorIds = Lens.lens (serviceErrorIds :: DescribeServiceErrors -> Lude.Maybe [Lude.Text]) (\s a -> s {serviceErrorIds = a} :: DescribeServiceErrors)
{-# DEPRECATED dseServiceErrorIds "Use generic-lens or generic-optics with 'serviceErrorIds' instead." #-}

instance Lude.AWSRequest DescribeServiceErrors where
  type Rs DescribeServiceErrors = DescribeServiceErrorsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeServiceErrorsResponse'
            Lude.<$> (x Lude..?> "ServiceErrors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeServiceErrors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeServiceErrors" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeServiceErrors where
  toJSON DescribeServiceErrors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceId" Lude..=) Lude.<$> instanceId,
            ("StackId" Lude..=) Lude.<$> stackId,
            ("ServiceErrorIds" Lude..=) Lude.<$> serviceErrorIds
          ]
      )

instance Lude.ToPath DescribeServiceErrors where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeServiceErrors where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeServiceErrors@ request.
--
-- /See:/ 'mkDescribeServiceErrorsResponse' smart constructor.
data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'
  { -- | An array of @ServiceError@ objects that describe the specified service errors.
    serviceErrors :: Lude.Maybe [ServiceError'],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeServiceErrorsResponse' with the minimum fields required to make a request.
--
-- * 'serviceErrors' - An array of @ServiceError@ objects that describe the specified service errors.
-- * 'responseStatus' - The response status code.
mkDescribeServiceErrorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeServiceErrorsResponse
mkDescribeServiceErrorsResponse pResponseStatus_ =
  DescribeServiceErrorsResponse'
    { serviceErrors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @ServiceError@ objects that describe the specified service errors.
--
-- /Note:/ Consider using 'serviceErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsersServiceErrors :: Lens.Lens' DescribeServiceErrorsResponse (Lude.Maybe [ServiceError'])
dsersServiceErrors = Lens.lens (serviceErrors :: DescribeServiceErrorsResponse -> Lude.Maybe [ServiceError']) (\s a -> s {serviceErrors = a} :: DescribeServiceErrorsResponse)
{-# DEPRECATED dsersServiceErrors "Use generic-lens or generic-optics with 'serviceErrors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsersResponseStatus :: Lens.Lens' DescribeServiceErrorsResponse Lude.Int
dsersResponseStatus = Lens.lens (responseStatus :: DescribeServiceErrorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeServiceErrorsResponse)
{-# DEPRECATED dsersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
