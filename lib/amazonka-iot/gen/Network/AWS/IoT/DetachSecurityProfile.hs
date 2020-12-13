{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Device Defender security profile from a thing group or from this account.
module Network.AWS.IoT.DetachSecurityProfile
  ( -- * Creating a request
    DetachSecurityProfile (..),
    mkDetachSecurityProfile,

    -- ** Request lenses
    dspSecurityProfileName,
    dspSecurityProfileTargetARN,

    -- * Destructuring the response
    DetachSecurityProfileResponse (..),
    mkDetachSecurityProfileResponse,

    -- ** Response lenses
    dspgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachSecurityProfile' smart constructor.
data DetachSecurityProfile = DetachSecurityProfile'
  { -- | The security profile that is detached.
    securityProfileName :: Lude.Text,
    -- | The ARN of the thing group from which the security profile is detached.
    securityProfileTargetARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachSecurityProfile' with the minimum fields required to make a request.
--
-- * 'securityProfileName' - The security profile that is detached.
-- * 'securityProfileTargetARN' - The ARN of the thing group from which the security profile is detached.
mkDetachSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  -- | 'securityProfileTargetARN'
  Lude.Text ->
  DetachSecurityProfile
mkDetachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetARN_ =
    DetachSecurityProfile'
      { securityProfileName =
          pSecurityProfileName_,
        securityProfileTargetARN = pSecurityProfileTargetARN_
      }

-- | The security profile that is detached.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspSecurityProfileName :: Lens.Lens' DetachSecurityProfile Lude.Text
dspSecurityProfileName = Lens.lens (securityProfileName :: DetachSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: DetachSecurityProfile)
{-# DEPRECATED dspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The ARN of the thing group from which the security profile is detached.
--
-- /Note:/ Consider using 'securityProfileTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspSecurityProfileTargetARN :: Lens.Lens' DetachSecurityProfile Lude.Text
dspSecurityProfileTargetARN = Lens.lens (securityProfileTargetARN :: DetachSecurityProfile -> Lude.Text) (\s a -> s {securityProfileTargetARN = a} :: DetachSecurityProfile)
{-# DEPRECATED dspSecurityProfileTargetARN "Use generic-lens or generic-optics with 'securityProfileTargetARN' instead." #-}

instance Lude.AWSRequest DetachSecurityProfile where
  type Rs DetachSecurityProfile = DetachSecurityProfileResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DetachSecurityProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachSecurityProfile where
  toPath DetachSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName, "/targets"]

instance Lude.ToQuery DetachSecurityProfile where
  toQuery DetachSecurityProfile' {..} =
    Lude.mconcat
      ["securityProfileTargetArn" Lude.=: securityProfileTargetARN]

-- | /See:/ 'mkDetachSecurityProfileResponse' smart constructor.
newtype DetachSecurityProfileResponse = DetachSecurityProfileResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDetachSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachSecurityProfileResponse
mkDetachSecurityProfileResponse pResponseStatus_ =
  DetachSecurityProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspgrsResponseStatus :: Lens.Lens' DetachSecurityProfileResponse Lude.Int
dspgrsResponseStatus = Lens.lens (responseStatus :: DetachSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachSecurityProfileResponse)
{-# DEPRECATED dspgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
