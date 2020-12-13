{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceProfile
  ( InstanceProfile (..),

    -- * Smart constructor
    mkInstanceProfile,

    -- * Lenses
    ipArn,
    ipRebootAfterUse,
    ipName,
    ipPackageCleanup,
    ipExcludeAppPackagesFromCleanup,
    ipDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the instance profile.
--
-- /See:/ 'mkInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Lude.Maybe Lude.Text,
    -- | When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
    rebootAfterUse :: Lude.Maybe Lude.Bool,
    -- | The name of the instance profile.
    name :: Lude.Maybe Lude.Text,
    -- | When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
    packageCleanup :: Lude.Maybe Lude.Bool,
    -- | An array of strings containing the list of app packages that should not be cleaned up from the device after a test run completes.
    --
    -- The list of packages is considered only if you set @packageCleanup@ to @true@ .
    excludeAppPackagesFromCleanup :: Lude.Maybe [Lude.Text],
    -- | The description of the instance profile.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceProfile' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance profile.
-- * 'rebootAfterUse' - When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
-- * 'name' - The name of the instance profile.
-- * 'packageCleanup' - When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
-- * 'excludeAppPackagesFromCleanup' - An array of strings containing the list of app packages that should not be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
-- * 'description' - The description of the instance profile.
mkInstanceProfile ::
  InstanceProfile
mkInstanceProfile =
  InstanceProfile'
    { arn = Lude.Nothing,
      rebootAfterUse = Lude.Nothing,
      name = Lude.Nothing,
      packageCleanup = Lude.Nothing,
      excludeAppPackagesFromCleanup = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipArn :: Lens.Lens' InstanceProfile (Lude.Maybe Lude.Text)
ipArn = Lens.lens (arn :: InstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InstanceProfile)
{-# DEPRECATED ipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When set to @true@ , Device Farm reboots the instance after a test run. The default value is @true@ .
--
-- /Note:/ Consider using 'rebootAfterUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipRebootAfterUse :: Lens.Lens' InstanceProfile (Lude.Maybe Lude.Bool)
ipRebootAfterUse = Lens.lens (rebootAfterUse :: InstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {rebootAfterUse = a} :: InstanceProfile)
{-# DEPRECATED ipRebootAfterUse "Use generic-lens or generic-optics with 'rebootAfterUse' instead." #-}

-- | The name of the instance profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipName :: Lens.Lens' InstanceProfile (Lude.Maybe Lude.Text)
ipName = Lens.lens (name :: InstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceProfile)
{-# DEPRECATED ipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When set to @true@ , Device Farm removes app packages after a test run. The default value is @false@ for private devices.
--
-- /Note:/ Consider using 'packageCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPackageCleanup :: Lens.Lens' InstanceProfile (Lude.Maybe Lude.Bool)
ipPackageCleanup = Lens.lens (packageCleanup :: InstanceProfile -> Lude.Maybe Lude.Bool) (\s a -> s {packageCleanup = a} :: InstanceProfile)
{-# DEPRECATED ipPackageCleanup "Use generic-lens or generic-optics with 'packageCleanup' instead." #-}

-- | An array of strings containing the list of app packages that should not be cleaned up from the device after a test run completes.
--
-- The list of packages is considered only if you set @packageCleanup@ to @true@ .
--
-- /Note:/ Consider using 'excludeAppPackagesFromCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipExcludeAppPackagesFromCleanup :: Lens.Lens' InstanceProfile (Lude.Maybe [Lude.Text])
ipExcludeAppPackagesFromCleanup = Lens.lens (excludeAppPackagesFromCleanup :: InstanceProfile -> Lude.Maybe [Lude.Text]) (\s a -> s {excludeAppPackagesFromCleanup = a} :: InstanceProfile)
{-# DEPRECATED ipExcludeAppPackagesFromCleanup "Use generic-lens or generic-optics with 'excludeAppPackagesFromCleanup' instead." #-}

-- | The description of the instance profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipDescription :: Lens.Lens' InstanceProfile (Lude.Maybe Lude.Text)
ipDescription = Lens.lens (description :: InstanceProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: InstanceProfile)
{-# DEPRECATED ipDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON InstanceProfile where
  parseJSON =
    Lude.withObject
      "InstanceProfile"
      ( \x ->
          InstanceProfile'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "rebootAfterUse")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "packageCleanup")
            Lude.<*> (x Lude..:? "excludeAppPackagesFromCleanup" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "description")
      )
