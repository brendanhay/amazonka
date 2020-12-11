{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified instance profile, including the instance profile's path, GUID, ARN, and role. For more information about instance profiles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> in the /IAM User Guide/ .
module Network.AWS.IAM.GetInstanceProfile
  ( -- * Creating a request
    GetInstanceProfile (..),
    mkGetInstanceProfile,

    -- ** Request lenses
    gipInstanceProfileName,

    -- * Destructuring the response
    GetInstanceProfileResponse (..),
    mkGetInstanceProfileResponse,

    -- ** Response lenses
    giprsResponseStatus,
    giprsInstanceProfile,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile'
  { instanceProfileName ::
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

-- | Creates a value of 'GetInstanceProfile' with the minimum fields required to make a request.
--
-- * 'instanceProfileName' - The name of the instance profile to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetInstanceProfile ::
  -- | 'instanceProfileName'
  Lude.Text ->
  GetInstanceProfile
mkGetInstanceProfile pInstanceProfileName_ =
  GetInstanceProfile' {instanceProfileName = pInstanceProfileName_}

-- | The name of the instance profile to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipInstanceProfileName :: Lens.Lens' GetInstanceProfile Lude.Text
gipInstanceProfileName = Lens.lens (instanceProfileName :: GetInstanceProfile -> Lude.Text) (\s a -> s {instanceProfileName = a} :: GetInstanceProfile)
{-# DEPRECATED gipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

instance Lude.AWSRequest GetInstanceProfile where
  type Rs GetInstanceProfile = GetInstanceProfileResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetInstanceProfileResult"
      ( \s h x ->
          GetInstanceProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "InstanceProfile")
      )

instance Lude.ToHeaders GetInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceProfile where
  toQuery GetInstanceProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "InstanceProfileName" Lude.=: instanceProfileName
      ]

-- | Contains the response to a successful 'GetInstanceProfile' request.
--
-- /See:/ 'mkGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { responseStatus ::
      Lude.Int,
    instanceProfile :: InstanceProfile
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfile' - A structure containing details about the instance profile.
-- * 'responseStatus' - The response status code.
mkGetInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'instanceProfile'
  InstanceProfile ->
  GetInstanceProfileResponse
mkGetInstanceProfileResponse pResponseStatus_ pInstanceProfile_ =
  GetInstanceProfileResponse'
    { responseStatus = pResponseStatus_,
      instanceProfile = pInstanceProfile_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsResponseStatus :: Lens.Lens' GetInstanceProfileResponse Lude.Int
giprsResponseStatus = Lens.lens (responseStatus :: GetInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceProfileResponse)
{-# DEPRECATED giprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsInstanceProfile :: Lens.Lens' GetInstanceProfileResponse InstanceProfile
giprsInstanceProfile = Lens.lens (instanceProfile :: GetInstanceProfileResponse -> InstanceProfile) (\s a -> s {instanceProfile = a} :: GetInstanceProfileResponse)
{-# DEPRECATED giprsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}
