{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
  ( OperatingSystemConfigurationManager (..),

    -- * Smart constructor
    mkOperatingSystemConfigurationManager,

    -- * Lenses
    oscmName,
    oscmVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.
--
-- /See:/ 'mkOperatingSystemConfigurationManager' smart constructor.
data OperatingSystemConfigurationManager = OperatingSystemConfigurationManager'
  { -- | The name of the configuration manager, which is Chef.
    name :: Lude.Maybe Lude.Text,
    -- | The versions of the configuration manager that are supported by an operating system.
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OperatingSystemConfigurationManager' with the minimum fields required to make a request.
--
-- * 'name' - The name of the configuration manager, which is Chef.
-- * 'version' - The versions of the configuration manager that are supported by an operating system.
mkOperatingSystemConfigurationManager ::
  OperatingSystemConfigurationManager
mkOperatingSystemConfigurationManager =
  OperatingSystemConfigurationManager'
    { name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name of the configuration manager, which is Chef.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscmName :: Lens.Lens' OperatingSystemConfigurationManager (Lude.Maybe Lude.Text)
oscmName = Lens.lens (name :: OperatingSystemConfigurationManager -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OperatingSystemConfigurationManager)
{-# DEPRECATED oscmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The versions of the configuration manager that are supported by an operating system.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscmVersion :: Lens.Lens' OperatingSystemConfigurationManager (Lude.Maybe Lude.Text)
oscmVersion = Lens.lens (version :: OperatingSystemConfigurationManager -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: OperatingSystemConfigurationManager)
{-# DEPRECATED oscmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON OperatingSystemConfigurationManager where
  parseJSON =
    Lude.withObject
      "OperatingSystemConfigurationManager"
      ( \x ->
          OperatingSystemConfigurationManager'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Version")
      )
