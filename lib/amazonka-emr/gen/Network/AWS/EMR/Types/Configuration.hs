{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Configuration
  ( Configuration (..),

    -- * Smart constructor
    mkConfiguration,

    -- * Lenses
    cConfigurations,
    cClassification,
    cProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
-- /See:/ 'mkConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | A list of additional configurations to apply within a configuration object.
    configurations :: Lude.Maybe [Configuration],
    -- | The classification within a configuration.
    classification :: Lude.Maybe Lude.Text,
    -- | A set of properties specified within a configuration classification.
    properties :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- * 'configurations' - A list of additional configurations to apply within a configuration object.
-- * 'classification' - The classification within a configuration.
-- * 'properties' - A set of properties specified within a configuration classification.
mkConfiguration ::
  Configuration
mkConfiguration =
  Configuration'
    { configurations = Lude.Nothing,
      classification = Lude.Nothing,
      properties = Lude.Nothing
    }

-- | A list of additional configurations to apply within a configuration object.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConfigurations :: Lens.Lens' Configuration (Lude.Maybe [Configuration])
cConfigurations = Lens.lens (configurations :: Configuration -> Lude.Maybe [Configuration]) (\s a -> s {configurations = a} :: Configuration)
{-# DEPRECATED cConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The classification within a configuration.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClassification :: Lens.Lens' Configuration (Lude.Maybe Lude.Text)
cClassification = Lens.lens (classification :: Configuration -> Lude.Maybe Lude.Text) (\s a -> s {classification = a} :: Configuration)
{-# DEPRECATED cClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | A set of properties specified within a configuration classification.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cProperties :: Lens.Lens' Configuration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cProperties = Lens.lens (properties :: Configuration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {properties = a} :: Configuration)
{-# DEPRECATED cProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

instance Lude.FromJSON Configuration where
  parseJSON =
    Lude.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Lude.<$> (x Lude..:? "Configurations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Classification")
            Lude.<*> (x Lude..:? "Properties" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Configuration where
  toJSON Configuration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Configurations" Lude..=) Lude.<$> configurations,
            ("Classification" Lude..=) Lude.<$> classification,
            ("Properties" Lude..=) Lude.<$> properties
          ]
      )
