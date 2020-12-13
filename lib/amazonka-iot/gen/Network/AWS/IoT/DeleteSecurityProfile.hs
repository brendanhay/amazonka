{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender security profile.
module Network.AWS.IoT.DeleteSecurityProfile
  ( -- * Creating a request
    DeleteSecurityProfile (..),
    mkDeleteSecurityProfile,

    -- ** Request lenses
    dspfExpectedVersion,
    dspfSecurityProfileName,

    -- * Destructuring the response
    DeleteSecurityProfileResponse (..),
    mkDeleteSecurityProfileResponse,

    -- ** Response lenses
    dsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSecurityProfile' smart constructor.
data DeleteSecurityProfile = DeleteSecurityProfile'
  { -- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the security profile to be deleted.
    securityProfileName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecurityProfile' with the minimum fields required to make a request.
--
-- * 'expectedVersion' - The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
-- * 'securityProfileName' - The name of the security profile to be deleted.
mkDeleteSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  DeleteSecurityProfile
mkDeleteSecurityProfile pSecurityProfileName_ =
  DeleteSecurityProfile'
    { expectedVersion = Lude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfExpectedVersion :: Lens.Lens' DeleteSecurityProfile (Lude.Maybe Lude.Integer)
dspfExpectedVersion = Lens.lens (expectedVersion :: DeleteSecurityProfile -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: DeleteSecurityProfile)
{-# DEPRECATED dspfExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the security profile to be deleted.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfSecurityProfileName :: Lens.Lens' DeleteSecurityProfile Lude.Text
dspfSecurityProfileName = Lens.lens (securityProfileName :: DeleteSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: DeleteSecurityProfile)
{-# DEPRECATED dspfSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

instance Lude.AWSRequest DeleteSecurityProfile where
  type Rs DeleteSecurityProfile = DeleteSecurityProfileResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSecurityProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSecurityProfile where
  toPath DeleteSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName]

instance Lude.ToQuery DeleteSecurityProfile where
  toQuery DeleteSecurityProfile' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | /See:/ 'mkDeleteSecurityProfileResponse' smart constructor.
newtype DeleteSecurityProfileResponse = DeleteSecurityProfileResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSecurityProfileResponse
mkDeleteSecurityProfileResponse pResponseStatus_ =
  DeleteSecurityProfileResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DeleteSecurityProfileResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DeleteSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSecurityProfileResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
