{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Device Defender security profile with a thing group or this account. Each thing group or account can have up to five security profiles associated with it.
module Network.AWS.IoT.AttachSecurityProfile
  ( -- * Creating a request
    AttachSecurityProfile (..),
    mkAttachSecurityProfile,

    -- ** Request lenses
    aspSecurityProfileName,
    aspSecurityProfileTargetARN,

    -- * Destructuring the response
    AttachSecurityProfileResponse (..),
    mkAttachSecurityProfileResponse,

    -- ** Response lenses
    asprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachSecurityProfile' smart constructor.
data AttachSecurityProfile = AttachSecurityProfile'
  { -- | The security profile that is attached.
    securityProfileName :: Lude.Text,
    -- | The ARN of the target (thing group) to which the security profile is attached.
    securityProfileTargetARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachSecurityProfile' with the minimum fields required to make a request.
--
-- * 'securityProfileName' - The security profile that is attached.
-- * 'securityProfileTargetARN' - The ARN of the target (thing group) to which the security profile is attached.
mkAttachSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  -- | 'securityProfileTargetARN'
  Lude.Text ->
  AttachSecurityProfile
mkAttachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetARN_ =
    AttachSecurityProfile'
      { securityProfileName =
          pSecurityProfileName_,
        securityProfileTargetARN = pSecurityProfileTargetARN_
      }

-- | The security profile that is attached.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspSecurityProfileName :: Lens.Lens' AttachSecurityProfile Lude.Text
aspSecurityProfileName = Lens.lens (securityProfileName :: AttachSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: AttachSecurityProfile)
{-# DEPRECATED aspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The ARN of the target (thing group) to which the security profile is attached.
--
-- /Note:/ Consider using 'securityProfileTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspSecurityProfileTargetARN :: Lens.Lens' AttachSecurityProfile Lude.Text
aspSecurityProfileTargetARN = Lens.lens (securityProfileTargetARN :: AttachSecurityProfile -> Lude.Text) (\s a -> s {securityProfileTargetARN = a} :: AttachSecurityProfile)
{-# DEPRECATED aspSecurityProfileTargetARN "Use generic-lens or generic-optics with 'securityProfileTargetARN' instead." #-}

instance Lude.AWSRequest AttachSecurityProfile where
  type Rs AttachSecurityProfile = AttachSecurityProfileResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AttachSecurityProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AttachSecurityProfile where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AttachSecurityProfile where
  toPath AttachSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName, "/targets"]

instance Lude.ToQuery AttachSecurityProfile where
  toQuery AttachSecurityProfile' {..} =
    Lude.mconcat
      ["securityProfileTargetArn" Lude.=: securityProfileTargetARN]

-- | /See:/ 'mkAttachSecurityProfileResponse' smart constructor.
newtype AttachSecurityProfileResponse = AttachSecurityProfileResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAttachSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachSecurityProfileResponse
mkAttachSecurityProfileResponse pResponseStatus_ =
  AttachSecurityProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asprsResponseStatus :: Lens.Lens' AttachSecurityProfileResponse Lude.Int
asprsResponseStatus = Lens.lens (responseStatus :: AttachSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachSecurityProfileResponse)
{-# DEPRECATED asprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
