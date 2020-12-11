{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an existing private device instance profile.
module Network.AWS.DeviceFarm.UpdateInstanceProfile
  ( -- * Creating a request
    UpdateInstanceProfile (..),
    mkUpdateInstanceProfile,

    -- ** Request lenses
    uipRebootAfterUse,
    uipName,
    uipPackageCleanup,
    uipExcludeAppPackagesFromCleanup,
    uipDescription,
    uipArn,

    -- * Destructuring the response
    UpdateInstanceProfileResponse (..),
    mkUpdateInstanceProfileResponse,

    -- ** Response lenses
    uiprsInstanceProfile,
    uiprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateInstanceProfile' smart constructor.
data UpdateInstanceProfile = UpdateInstanceProfile'
  { rebootAfterUse ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    packageCleanup :: Lude.Maybe Lude.Bool,
    excludeAppPackagesFromCleanup ::
      Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'description' - The updated description for your instance profile.
-- * 'excludeAppPackagesFromCleanup' - An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to @true@ .
-- * 'name' - The updated name for your instance profile.
-- * 'packageCleanup' - The updated choice for whether you want to specify package cleanup. The default value is @false@ for private devices.
-- * 'rebootAfterUse' - The updated choice for whether you want to reboot the device after use. The default value is @true@ .
mkUpdateInstanceProfile ::
  -- | 'arn'
  Lude.Text ->
  UpdateInstanceProfile
mkUpdateInstanceProfile pArn_ =
  UpdateInstanceProfile'
    { rebootAfterUse = Lude.Nothing,
      name = Lude.Nothing,
      packageCleanup = Lude.Nothing,
      excludeAppPackagesFromCleanup = Lude.Nothing,
      description = Lude.Nothing,
      arn = pArn_
    }

-- | The updated choice for whether you want to reboot the device after use. The default value is @true@ .
--
-- /Note:/ Consider using 'rebootAfterUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipRebootAfterUse :: Lens.Lens' UpdateInstanceProfile (Lude.Maybe Lude.Bool)
uipRebootAfterUse = Lens.lens (rebootAfterUse :: UpdateInstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {rebootAfterUse = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipRebootAfterUse "Use generic-lens or generic-optics with 'rebootAfterUse' instead." #-}

-- | The updated name for your instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipName :: Lens.Lens' UpdateInstanceProfile (Lude.Maybe Lude.Text)
uipName = Lens.lens (name :: UpdateInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated choice for whether you want to specify package cleanup. The default value is @false@ for private devices.
--
-- /Note:/ Consider using 'packageCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipPackageCleanup :: Lens.Lens' UpdateInstanceProfile (Lude.Maybe Lude.Bool)
uipPackageCleanup = Lens.lens (packageCleanup :: UpdateInstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {packageCleanup = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipPackageCleanup "Use generic-lens or generic-optics with 'packageCleanup' instead." #-}

-- | An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run is over.
--
-- The list of packages is only considered if you set @packageCleanup@ to @true@ .
--
-- /Note:/ Consider using 'excludeAppPackagesFromCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipExcludeAppPackagesFromCleanup :: Lens.Lens' UpdateInstanceProfile (Lude.Maybe [Lude.Text])
uipExcludeAppPackagesFromCleanup = Lens.lens (excludeAppPackagesFromCleanup :: UpdateInstanceProfile -> Lude.Maybe [Lude.Text]) (\s a -> s {excludeAppPackagesFromCleanup = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipExcludeAppPackagesFromCleanup "Use generic-lens or generic-optics with 'excludeAppPackagesFromCleanup' instead." #-}

-- | The updated description for your instance profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipDescription :: Lens.Lens' UpdateInstanceProfile (Lude.Maybe Lude.Text)
uipDescription = Lens.lens (description :: UpdateInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipArn :: Lens.Lens' UpdateInstanceProfile Lude.Text
uipArn = Lens.lens (arn :: UpdateInstanceProfile -> Lude.Text) (\s a -> s {arn = a} :: UpdateInstanceProfile)
{-# DEPRECATED uipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateInstanceProfile where
  type Rs UpdateInstanceProfile = UpdateInstanceProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateInstanceProfileResponse'
            Lude.<$> (x Lude..?> "instanceProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateInstanceProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateInstanceProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInstanceProfile where
  toJSON UpdateInstanceProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rebootAfterUse" Lude..=) Lude.<$> rebootAfterUse,
            ("name" Lude..=) Lude.<$> name,
            ("packageCleanup" Lude..=) Lude.<$> packageCleanup,
            ("excludeAppPackagesFromCleanup" Lude..=)
              Lude.<$> excludeAppPackagesFromCleanup,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateInstanceProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateInstanceProfileResponse' smart constructor.
data UpdateInstanceProfileResponse = UpdateInstanceProfileResponse'
  { instanceProfile ::
      Lude.Maybe InstanceProfile,
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

-- | Creates a value of 'UpdateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfile' - An object that contains information about your instance profile.
-- * 'responseStatus' - The response status code.
mkUpdateInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateInstanceProfileResponse
mkUpdateInstanceProfileResponse pResponseStatus_ =
  UpdateInstanceProfileResponse'
    { instanceProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprsInstanceProfile :: Lens.Lens' UpdateInstanceProfileResponse (Lude.Maybe InstanceProfile)
uiprsInstanceProfile = Lens.lens (instanceProfile :: UpdateInstanceProfileResponse -> Lude.Maybe InstanceProfile) (\s a -> s {instanceProfile = a} :: UpdateInstanceProfileResponse)
{-# DEPRECATED uiprsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprsResponseStatus :: Lens.Lens' UpdateInstanceProfileResponse Lude.Int
uiprsResponseStatus = Lens.lens (responseStatus :: UpdateInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateInstanceProfileResponse)
{-# DEPRECATED uiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
