{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance profile. For information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateInstanceProfile
  ( -- * Creating a request
    CreateInstanceProfile (..),
    mkCreateInstanceProfile,

    -- ** Request lenses
    cipPath,
    cipInstanceProfileName,

    -- * Destructuring the response
    CreateInstanceProfileResponse (..),
    mkCreateInstanceProfileResponse,

    -- ** Response lenses
    ciprsInstanceProfile,
    ciprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { -- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    --
    -- This parameter is optional. If it is not included, it defaults to a slash (/).
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    path :: Lude.Maybe Lude.Text,
    -- | The name of the instance profile to create.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    instanceProfileName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceProfile' with the minimum fields required to make a request.
--
-- * 'path' - The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'instanceProfileName' - The name of the instance profile to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkCreateInstanceProfile ::
  -- | 'instanceProfileName'
  Lude.Text ->
  CreateInstanceProfile
mkCreateInstanceProfile pInstanceProfileName_ =
  CreateInstanceProfile'
    { path = Lude.Nothing,
      instanceProfileName = pInstanceProfileName_
    }

-- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipPath :: Lens.Lens' CreateInstanceProfile (Lude.Maybe Lude.Text)
cipPath = Lens.lens (path :: CreateInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreateInstanceProfile)
{-# DEPRECATED cipPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the instance profile to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipInstanceProfileName :: Lens.Lens' CreateInstanceProfile Lude.Text
cipInstanceProfileName = Lens.lens (instanceProfileName :: CreateInstanceProfile -> Lude.Text) (\s a -> s {instanceProfileName = a} :: CreateInstanceProfile)
{-# DEPRECATED cipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

instance Lude.AWSRequest CreateInstanceProfile where
  type Rs CreateInstanceProfile = CreateInstanceProfileResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateInstanceProfileResult"
      ( \s h x ->
          CreateInstanceProfileResponse'
            Lude.<$> (x Lude..@ "InstanceProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstanceProfile where
  toQuery CreateInstanceProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Path" Lude.=: path,
        "InstanceProfileName" Lude.=: instanceProfileName
      ]

-- | Contains the response to a successful 'CreateInstanceProfile' request.
--
-- /See:/ 'mkCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { -- | A structure containing details about the new instance profile.
    instanceProfile :: InstanceProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfile' - A structure containing details about the new instance profile.
-- * 'responseStatus' - The response status code.
mkCreateInstanceProfileResponse ::
  -- | 'instanceProfile'
  InstanceProfile ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceProfileResponse
mkCreateInstanceProfileResponse pInstanceProfile_ pResponseStatus_ =
  CreateInstanceProfileResponse'
    { instanceProfile =
        pInstanceProfile_,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the new instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsInstanceProfile :: Lens.Lens' CreateInstanceProfileResponse InstanceProfile
ciprsInstanceProfile = Lens.lens (instanceProfile :: CreateInstanceProfileResponse -> InstanceProfile) (\s a -> s {instanceProfile = a} :: CreateInstanceProfileResponse)
{-# DEPRECATED ciprsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsResponseStatus :: Lens.Lens' CreateInstanceProfileResponse Lude.Int
ciprsResponseStatus = Lens.lens (responseStatus :: CreateInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceProfileResponse)
{-# DEPRECATED ciprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
