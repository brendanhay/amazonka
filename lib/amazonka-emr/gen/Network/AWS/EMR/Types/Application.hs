{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Application
  ( Application (..),

    -- * Smart constructor
    mkApplication,

    -- * Lenses
    aAdditionalInfo,
    aArgs,
    aName,
    aVersion,
  )
where

import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.EMR.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | With Amazon EMR release version 4.0 and later, the only accepted parameter is the application name. To pass arguments to applications, you use configuration classifications specified using configuration JSON objects. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
-- With earlier Amazon EMR releases, the application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { -- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
    additionalInfo :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Arguments for Amazon EMR to pass to the application.
    args :: Core.Maybe [Types.String],
    -- | The name of the application.
    name :: Core.Maybe Types.Name,
    -- | The version of the application.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Application' value with any optional fields omitted.
mkApplication ::
  Application
mkApplication =
  Application'
    { additionalInfo = Core.Nothing,
      args = Core.Nothing,
      name = Core.Nothing,
      version = Core.Nothing
    }

-- | This option is for advanced users only. This is meta information about third-party applications that third-party vendors use for testing purposes.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAdditionalInfo :: Lens.Lens' Application (Core.Maybe (Core.HashMap Types.String Types.String))
aAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED aAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | Arguments for Amazon EMR to pass to the application.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArgs :: Lens.Lens' Application (Core.Maybe [Types.String])
aArgs = Lens.field @"args"
{-# DEPRECATED aArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Application (Core.Maybe Types.Name)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aVersion :: Lens.Lens' Application (Core.Maybe Types.Version)
aVersion = Lens.field @"version"
{-# DEPRECATED aVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON Application where
  toJSON Application {..} =
    Core.object
      ( Core.catMaybes
          [ ("AdditionalInfo" Core..=) Core.<$> additionalInfo,
            ("Args" Core..=) Core.<$> args,
            ("Name" Core..=) Core.<$> name,
            ("Version" Core..=) Core.<$> version
          ]
      )

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject "Application" Core.$
      \x ->
        Application'
          Core.<$> (x Core..:? "AdditionalInfo")
          Core.<*> (x Core..:? "Args")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Version")
