{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile that can be applied to one or more private fleet device instances.
module Network.AWS.DeviceFarm.CreateInstanceProfile
  ( -- * Creating a request
    CreateInstanceProfile (..),
    mkCreateInstanceProfile,

    -- ** Request lenses
    cipRebootAfterUse,
    cipPackageCleanup,
    cipExcludeAppPackagesFromCleanup,
    cipDescription,
    cipName,

    -- * Destructuring the response
    CreateInstanceProfileResponse (..),
    mkCreateInstanceProfileResponse,

    -- ** Response lenses
    ciprsInstanceProfile,
    ciprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { rebootAfterUse ::
      Lude.Maybe Lude.Bool,
    packageCleanup :: Lude.Maybe Lude.Bool,
    excludeAppPackagesFromCleanup ::
      Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceProfile' with the minimum fields required to make a request.
--
-- * 'description' - The description of your instance profile.
-- * 'excludeAppPackagesFromCleanup' - An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
-- * 'name' - The name of your instance profile.
-- * 'packageCleanup' - When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
-- * 'rebootAfterUse' - When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
mkCreateInstanceProfile ::
  -- | 'name'
  Lude.Text ->
  CreateInstanceProfile
mkCreateInstanceProfile pName_ =
  CreateInstanceProfile'
    { rebootAfterUse = Lude.Nothing,
      packageCleanup = Lude.Nothing,
      excludeAppPackagesFromCleanup = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_
    }

-- | When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
--
-- /Note:/ Consider using 'rebootAfterUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipRebootAfterUse :: Lens.Lens' CreateInstanceProfile (Lude.Maybe Lude.Bool)
cipRebootAfterUse = Lens.lens (rebootAfterUse :: CreateInstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {rebootAfterUse = a} :: CreateInstanceProfile)
{-# DEPRECATED cipRebootAfterUse "Use generic-lens or generic-optics with 'rebootAfterUse' instead." #-}

-- | When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
--
-- /Note:/ Consider using 'packageCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipPackageCleanup :: Lens.Lens' CreateInstanceProfile (Lude.Maybe Lude.Bool)
cipPackageCleanup = Lens.lens (packageCleanup :: CreateInstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {packageCleanup = a} :: CreateInstanceProfile)
{-# DEPRECATED cipPackageCleanup "Use generic-lens or generic-optics with 'packageCleanup' instead." #-}

-- | An array of strings that specifies the list of app packages that should not be cleaned up from the device after a test run.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
--
-- /Note:/ Consider using 'excludeAppPackagesFromCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipExcludeAppPackagesFromCleanup :: Lens.Lens' CreateInstanceProfile (Lude.Maybe [Lude.Text])
cipExcludeAppPackagesFromCleanup = Lens.lens (excludeAppPackagesFromCleanup :: CreateInstanceProfile -> Lude.Maybe [Lude.Text]) (\s a -> s {excludeAppPackagesFromCleanup = a} :: CreateInstanceProfile)
{-# DEPRECATED cipExcludeAppPackagesFromCleanup "Use generic-lens or generic-optics with 'excludeAppPackagesFromCleanup' instead." #-}

-- | The description of your instance profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipDescription :: Lens.Lens' CreateInstanceProfile (Lude.Maybe Lude.Text)
cipDescription = Lens.lens (description :: CreateInstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateInstanceProfile)
{-# DEPRECATED cipDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of your instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipName :: Lens.Lens' CreateInstanceProfile Lude.Text
cipName = Lens.lens (name :: CreateInstanceProfile -> Lude.Text) (\s a -> s {name = a} :: CreateInstanceProfile)
{-# DEPRECATED cipName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateInstanceProfile where
  type Rs CreateInstanceProfile = CreateInstanceProfileResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstanceProfileResponse'
            Lude.<$> (x Lude..?> "instanceProfile")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstanceProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateInstanceProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstanceProfile where
  toJSON CreateInstanceProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rebootAfterUse" Lude..=) Lude.<$> rebootAfterUse,
            ("packageCleanup" Lude..=) Lude.<$> packageCleanup,
            ("excludeAppPackagesFromCleanup" Lude..=)
              Lude.<$> excludeAppPackagesFromCleanup,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstanceProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
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

-- | Creates a value of 'CreateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfile' - An object that contains information about your instance profile.
-- * 'responseStatus' - The response status code.
mkCreateInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceProfileResponse
mkCreateInstanceProfileResponse pResponseStatus_ =
  CreateInstanceProfileResponse'
    { instanceProfile = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains information about your instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsInstanceProfile :: Lens.Lens' CreateInstanceProfileResponse (Lude.Maybe InstanceProfile)
ciprsInstanceProfile = Lens.lens (instanceProfile :: CreateInstanceProfileResponse -> Lude.Maybe InstanceProfile) (\s a -> s {instanceProfile = a} :: CreateInstanceProfileResponse)
{-# DEPRECATED ciprsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprsResponseStatus :: Lens.Lens' CreateInstanceProfileResponse Lude.Int
ciprsResponseStatus = Lens.lens (responseStatus :: CreateInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceProfileResponse)
{-# DEPRECATED ciprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
