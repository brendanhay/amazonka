{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.JupyterServerAppSettings
  ( JupyterServerAppSettings (..),

    -- * Smart constructor
    mkJupyterServerAppSettings,

    -- * Lenses
    jsasDefaultResourceSpec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ResourceSpec as Types

-- | The JupyterServer app settings.
--
-- /See:/ 'mkJupyterServerAppSettings' smart constructor.
newtype JupyterServerAppSettings = JupyterServerAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the JupyterServer app.
    defaultResourceSpec :: Core.Maybe Types.ResourceSpec
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JupyterServerAppSettings' value with any optional fields omitted.
mkJupyterServerAppSettings ::
  JupyterServerAppSettings
mkJupyterServerAppSettings =
  JupyterServerAppSettings' {defaultResourceSpec = Core.Nothing}

-- | The default instance type and the Amazon Resource Name (ARN) of the default SageMaker image used by the JupyterServer app.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsasDefaultResourceSpec :: Lens.Lens' JupyterServerAppSettings (Core.Maybe Types.ResourceSpec)
jsasDefaultResourceSpec = Lens.field @"defaultResourceSpec"
{-# DEPRECATED jsasDefaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead." #-}

instance Core.FromJSON JupyterServerAppSettings where
  toJSON JupyterServerAppSettings {..} =
    Core.object
      ( Core.catMaybes
          [("DefaultResourceSpec" Core..=) Core.<$> defaultResourceSpec]
      )

instance Core.FromJSON JupyterServerAppSettings where
  parseJSON =
    Core.withObject "JupyterServerAppSettings" Core.$
      \x ->
        JupyterServerAppSettings'
          Core.<$> (x Core..:? "DefaultResourceSpec")
