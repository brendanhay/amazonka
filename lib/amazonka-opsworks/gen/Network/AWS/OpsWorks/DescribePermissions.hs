{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribePermissions
  ( -- * Creating a request
    DescribePermissions (..),
    mkDescribePermissions,

    -- ** Request lenses
    dpIAMUserARN,
    dpStackId,

    -- * Destructuring the response
    DescribePermissionsResponse (..),
    mkDescribePermissionsResponse,

    -- ** Response lenses
    dprsPermissions,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePermissions' smart constructor.
data DescribePermissions = DescribePermissions'
  { -- | The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    iamUserARN :: Lude.Maybe Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePermissions' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'stackId' - The stack ID.
mkDescribePermissions ::
  DescribePermissions
mkDescribePermissions =
  DescribePermissions'
    { iamUserARN = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpIAMUserARN :: Lens.Lens' DescribePermissions (Lude.Maybe Lude.Text)
dpIAMUserARN = Lens.lens (iamUserARN :: DescribePermissions -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: DescribePermissions)
{-# DEPRECATED dpIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpStackId :: Lens.Lens' DescribePermissions (Lude.Maybe Lude.Text)
dpStackId = Lens.lens (stackId :: DescribePermissions -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribePermissions)
{-# DEPRECATED dpStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribePermissions where
  type Rs DescribePermissions = DescribePermissionsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePermissionsResponse'
            Lude.<$> (x Lude..?> "Permissions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePermissions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribePermissions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePermissions where
  toJSON DescribePermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IamUserArn" Lude..=) Lude.<$> iamUserARN,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribePermissions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePermissions where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribePermissions@ request.
--
-- /See:/ 'mkDescribePermissionsResponse' smart constructor.
data DescribePermissionsResponse = DescribePermissionsResponse'
  { -- | An array of @Permission@ objects that describe the stack permissions.
    --
    --
    --     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.
    --
    --
    --     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.
    --
    --
    --     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
    permissions :: Lude.Maybe [Permission],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePermissionsResponse' with the minimum fields required to make a request.
--
-- * 'permissions' - An array of @Permission@ objects that describe the stack permissions.
--
--
--     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.
--
--
--     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.
--
--
--     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
--
--
-- * 'responseStatus' - The response status code.
mkDescribePermissionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePermissionsResponse
mkDescribePermissionsResponse pResponseStatus_ =
  DescribePermissionsResponse'
    { permissions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @Permission@ objects that describe the stack permissions.
--
--
--     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.
--
--
--     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.
--
--
--     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsPermissions :: Lens.Lens' DescribePermissionsResponse (Lude.Maybe [Permission])
dprsPermissions = Lens.lens (permissions :: DescribePermissionsResponse -> Lude.Maybe [Permission]) (\s a -> s {permissions = a} :: DescribePermissionsResponse)
{-# DEPRECATED dprsPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePermissionsResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePermissionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePermissionsResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
