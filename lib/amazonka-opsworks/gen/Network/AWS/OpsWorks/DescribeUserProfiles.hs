{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe specified users.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeUserProfiles
  ( -- * Creating a request
    DescribeUserProfiles (..),
    mkDescribeUserProfiles,

    -- ** Request lenses
    dupIAMUserARNs,

    -- * Destructuring the response
    DescribeUserProfilesResponse (..),
    mkDescribeUserProfilesResponse,

    -- ** Response lenses
    duprsUserProfiles,
    duprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserProfiles' smart constructor.
newtype DescribeUserProfiles = DescribeUserProfiles'
  { iamUserARNs ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserProfiles' with the minimum fields required to make a request.
--
-- * 'iamUserARNs' - An array of IAM or federated user ARNs that identify the users to be described.
mkDescribeUserProfiles ::
  DescribeUserProfiles
mkDescribeUserProfiles =
  DescribeUserProfiles' {iamUserARNs = Lude.Nothing}

-- | An array of IAM or federated user ARNs that identify the users to be described.
--
-- /Note:/ Consider using 'iamUserARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupIAMUserARNs :: Lens.Lens' DescribeUserProfiles (Lude.Maybe [Lude.Text])
dupIAMUserARNs = Lens.lens (iamUserARNs :: DescribeUserProfiles -> Lude.Maybe [Lude.Text]) (\s a -> s {iamUserARNs = a} :: DescribeUserProfiles)
{-# DEPRECATED dupIAMUserARNs "Use generic-lens or generic-optics with 'iamUserARNs' instead." #-}

instance Lude.AWSRequest DescribeUserProfiles where
  type Rs DescribeUserProfiles = DescribeUserProfilesResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserProfilesResponse'
            Lude.<$> (x Lude..?> "UserProfiles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeUserProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserProfiles where
  toJSON DescribeUserProfiles' {..} =
    Lude.object
      (Lude.catMaybes [("IamUserArns" Lude..=) Lude.<$> iamUserARNs])

instance Lude.ToPath DescribeUserProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserProfiles where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeUserProfiles@ request.
--
-- /See:/ 'mkDescribeUserProfilesResponse' smart constructor.
data DescribeUserProfilesResponse = DescribeUserProfilesResponse'
  { userProfiles ::
      Lude.Maybe [UserProfile],
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

-- | Creates a value of 'DescribeUserProfilesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userProfiles' - A @Users@ object that describes the specified users.
mkDescribeUserProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserProfilesResponse
mkDescribeUserProfilesResponse pResponseStatus_ =
  DescribeUserProfilesResponse'
    { userProfiles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @Users@ object that describes the specified users.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserProfiles :: Lens.Lens' DescribeUserProfilesResponse (Lude.Maybe [UserProfile])
duprsUserProfiles = Lens.lens (userProfiles :: DescribeUserProfilesResponse -> Lude.Maybe [UserProfile]) (\s a -> s {userProfiles = a} :: DescribeUserProfilesResponse)
{-# DEPRECATED duprsUserProfiles "Use generic-lens or generic-optics with 'userProfiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsResponseStatus :: Lens.Lens' DescribeUserProfilesResponse Lude.Int
duprsResponseStatus = Lens.lens (responseStatus :: DescribeUserProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserProfilesResponse)
{-# DEPRECATED duprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
