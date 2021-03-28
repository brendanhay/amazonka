{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Configuration
  ( Configuration (..)
  -- * Smart constructor
  , mkConfiguration
  -- * Lenses
  , cClassification
  , cConfigurations
  , cProperties
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
-- /See:/ 'mkConfiguration' smart constructor.
data Configuration = Configuration'
  { classification :: Core.Maybe Core.Text
    -- ^ The classification within a configuration.
  , configurations :: Core.Maybe [Configuration]
    -- ^ A list of additional configurations to apply within a configuration object.
  , properties :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A set of properties specified within a configuration classification.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Configuration' value with any optional fields omitted.
mkConfiguration
    :: Configuration
mkConfiguration
  = Configuration'{classification = Core.Nothing,
                   configurations = Core.Nothing, properties = Core.Nothing}

-- | The classification within a configuration.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClassification :: Lens.Lens' Configuration (Core.Maybe Core.Text)
cClassification = Lens.field @"classification"
{-# INLINEABLE cClassification #-}
{-# DEPRECATED classification "Use generic-lens or generic-optics with 'classification' instead"  #-}

-- | A list of additional configurations to apply within a configuration object.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConfigurations :: Lens.Lens' Configuration (Core.Maybe [Configuration])
cConfigurations = Lens.field @"configurations"
{-# INLINEABLE cConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | A set of properties specified within a configuration classification.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cProperties :: Lens.Lens' Configuration (Core.Maybe (Core.HashMap Core.Text Core.Text))
cProperties = Lens.field @"properties"
{-# INLINEABLE cProperties #-}
{-# DEPRECATED properties "Use generic-lens or generic-optics with 'properties' instead"  #-}

instance Core.FromJSON Configuration where
        toJSON Configuration{..}
          = Core.object
              (Core.catMaybes
                 [("Classification" Core..=) Core.<$> classification,
                  ("Configurations" Core..=) Core.<$> configurations,
                  ("Properties" Core..=) Core.<$> properties])

instance Core.FromJSON Configuration where
        parseJSON
          = Core.withObject "Configuration" Core.$
              \ x ->
                Configuration' Core.<$>
                  (x Core..:? "Classification") Core.<*> x Core..:? "Configurations"
                    Core.<*> x Core..:? "Properties"
