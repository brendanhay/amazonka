{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user's SSH information.
--
-- __Required Permissions__ : To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeMyUserProfile
  ( -- * Creating a request
    DescribeMyUserProfile (..),
    mkDescribeMyUserProfile,

    -- * Destructuring the response
    DescribeMyUserProfileResponse (..),
    mkDescribeMyUserProfileResponse,

    -- ** Response lenses
    dmuprsUserProfile,
    dmuprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMyUserProfile' smart constructor.
data DescribeMyUserProfile = DescribeMyUserProfile'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMyUserProfile' with the minimum fields required to make a request.
mkDescribeMyUserProfile ::
  DescribeMyUserProfile
mkDescribeMyUserProfile = DescribeMyUserProfile'

instance Lude.AWSRequest DescribeMyUserProfile where
  type Rs DescribeMyUserProfile = DescribeMyUserProfileResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMyUserProfileResponse'
            Lude.<$> (x Lude..?> "UserProfile") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMyUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeMyUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMyUserProfile where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeMyUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMyUserProfile where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeMyUserProfile@ request.
--
-- /See:/ 'mkDescribeMyUserProfileResponse' smart constructor.
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
  { -- | A @UserProfile@ object that describes the user's SSH information.
    userProfile :: Lude.Maybe SelfUserProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMyUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'userProfile' - A @UserProfile@ object that describes the user's SSH information.
-- * 'responseStatus' - The response status code.
mkDescribeMyUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMyUserProfileResponse
mkDescribeMyUserProfileResponse pResponseStatus_ =
  DescribeMyUserProfileResponse'
    { userProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @UserProfile@ object that describes the user's SSH information.
--
-- /Note:/ Consider using 'userProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmuprsUserProfile :: Lens.Lens' DescribeMyUserProfileResponse (Lude.Maybe SelfUserProfile)
dmuprsUserProfile = Lens.lens (userProfile :: DescribeMyUserProfileResponse -> Lude.Maybe SelfUserProfile) (\s a -> s {userProfile = a} :: DescribeMyUserProfileResponse)
{-# DEPRECATED dmuprsUserProfile "Use generic-lens or generic-optics with 'userProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmuprsResponseStatus :: Lens.Lens' DescribeMyUserProfileResponse Lude.Int
dmuprsResponseStatus = Lens.lens (responseStatus :: DescribeMyUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMyUserProfileResponse)
{-# DEPRECATED dmuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
