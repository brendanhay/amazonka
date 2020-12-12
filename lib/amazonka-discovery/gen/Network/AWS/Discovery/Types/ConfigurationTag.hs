{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationTag
  ( ConfigurationTag (..),

    -- * Smart constructor
    mkConfigurationTag,

    -- * Lenses
    ctTimeOfCreation,
    ctConfigurationId,
    ctConfigurationType,
    ctValue,
    ctKey,
  )
where

import Network.AWS.Discovery.Types.ConfigurationItemType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Tags for a configuration item. Tags are metadata that help you categorize IT assets.
--
-- /See:/ 'mkConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { timeOfCreation ::
      Lude.Maybe Lude.Timestamp,
    configurationId :: Lude.Maybe Lude.Text,
    configurationType :: Lude.Maybe ConfigurationItemType,
    value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationTag' with the minimum fields required to make a request.
--
-- * 'configurationId' - The configuration ID for the item to tag. You can specify a list of keys and values.
-- * 'configurationType' - A type of IT asset to tag.
-- * 'key' - A type of tag on which to filter. For example, /serverType/ .
-- * 'timeOfCreation' - The time the configuration tag was created in Coordinated Universal Time (UTC).
-- * 'value' - A value on which to filter. For example /key = serverType/ and /value = web server/ .
mkConfigurationTag ::
  ConfigurationTag
mkConfigurationTag =
  ConfigurationTag'
    { timeOfCreation = Lude.Nothing,
      configurationId = Lude.Nothing,
      configurationType = Lude.Nothing,
      value = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The time the configuration tag was created in Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'timeOfCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTimeOfCreation :: Lens.Lens' ConfigurationTag (Lude.Maybe Lude.Timestamp)
ctTimeOfCreation = Lens.lens (timeOfCreation :: ConfigurationTag -> Lude.Maybe Lude.Timestamp) (\s a -> s {timeOfCreation = a} :: ConfigurationTag)
{-# DEPRECATED ctTimeOfCreation "Use generic-lens or generic-optics with 'timeOfCreation' instead." #-}

-- | The configuration ID for the item to tag. You can specify a list of keys and values.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationId :: Lens.Lens' ConfigurationTag (Lude.Maybe Lude.Text)
ctConfigurationId = Lens.lens (configurationId :: ConfigurationTag -> Lude.Maybe Lude.Text) (\s a -> s {configurationId = a} :: ConfigurationTag)
{-# DEPRECATED ctConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

-- | A type of IT asset to tag.
--
-- /Note:/ Consider using 'configurationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConfigurationType :: Lens.Lens' ConfigurationTag (Lude.Maybe ConfigurationItemType)
ctConfigurationType = Lens.lens (configurationType :: ConfigurationTag -> Lude.Maybe ConfigurationItemType) (\s a -> s {configurationType = a} :: ConfigurationTag)
{-# DEPRECATED ctConfigurationType "Use generic-lens or generic-optics with 'configurationType' instead." #-}

-- | A value on which to filter. For example /key = serverType/ and /value = web server/ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctValue :: Lens.Lens' ConfigurationTag (Lude.Maybe Lude.Text)
ctValue = Lens.lens (value :: ConfigurationTag -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ConfigurationTag)
{-# DEPRECATED ctValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A type of tag on which to filter. For example, /serverType/ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctKey :: Lens.Lens' ConfigurationTag (Lude.Maybe Lude.Text)
ctKey = Lens.lens (key :: ConfigurationTag -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ConfigurationTag)
{-# DEPRECATED ctKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ConfigurationTag where
  parseJSON =
    Lude.withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            Lude.<$> (x Lude..:? "timeOfCreation")
            Lude.<*> (x Lude..:? "configurationId")
            Lude.<*> (x Lude..:? "configurationType")
            Lude.<*> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "key")
      )
